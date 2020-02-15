package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.{Attempt, Err}
import scodec.bits.BitVector

sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, rawStream: BitVector, stream: Uncompressed)
  extends Decoded

  case class Meta(xrefs: NonEmptyList[Xref], trailer: Trailer, version: Version)
  extends Decoded
}

object Decode
{
  case class State(xrefs: List[Xref], version: Option[Version])

  def decodeObjectStream[A](stream: Uncompressed)(data: Prim)
  : Option[Attempt[Either[A, List[Decoded]]]] =
    Content.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(a => Right(a.map(Decoded.DataObj(_)))))

  def trailer(data: Prim.Dict): Attempt[Trailer] =
    Attempt.fromOption(
      Prim.path("Size")(data) { case Prim.Number(size) => Trailer(size, data) },
      Err("no Size in xref stream data"),
    )

  def extractMetadata(stream: Uncompressed)
  : Prim => Option[Attempt[Either[Xref, List[Decoded]]]] = {
    case Prim.tpe("XRef", data) =>
      Some(stream.exec.flatMap(XrefStream(data)).map(xs => Left(Xref(xs.tables, xs.trailer, 0))))
    case _ =>
      None
  }

  def analyzeStream
  (index: Obj.Index, data: Prim)
  (rawStream: BitVector, stream: Uncompressed)
  : Attempt[Either[Xref, List[Decoded]]] =
    decodeObjectStream(stream)(data)
      .orElse(extractMetadata(stream)(data))
      .getOrElse(Attempt.successful(Right(List(Decoded.ContentObj(Obj(index, data), rawStream, stream)))))

  def contentObj
  (state: State)
  (index: Obj.Index, data: Prim, stream: BitVector)
  : Pull[IO, Decoded, State] =
    StreamUtil.attemptPullWith("extract stream objects")(
      analyzeStream(index, data)(stream, Content.uncompress(stream)(data))
    ) {
      case Right(decoded) => Pull.output(Chunk.seq(decoded)).as(state)
      case Left(xref) => Pull.pure(state.copy(xrefs = xref :: state.xrefs))
    }

  def pullTopLevel(state: State): TopLevel => Pull[IO, Decoded, State] = {
    case TopLevel.IndirectObj(IndirectObj(index, data, Some(stream))) =>
      contentObj(state)(index, data, stream)
    case TopLevel.IndirectObj(IndirectObj(_, Prim.Dict(data), None)) if data.contains("Linearized") =>
      Pull.pure(state)
    case TopLevel.IndirectObj(IndirectObj(index, data, None)) =>
      Pull.output1(Decoded.DataObj(Obj(index, data))).as(state)
    case TopLevel.Version(version) =>
      Pull.pure(state.copy(version = Some(version)))
    case TopLevel.Xref(xref) =>
      Pull.pure(state.copy(xrefs = xref :: state.xrefs))
    case TopLevel.StartXref(_) =>
      Pull.pure(state)
    case TopLevel.Comment(_) =>
      Pull.pure(state)
  }

  def decodeTopLevelPull(in: Stream[IO, TopLevel]): Pull[IO, Decoded, Unit] =
    StreamUtil.pullState(pullTopLevel)(in)(State(Nil, None))
      .flatMap {
        case State(h :: t, Some(version)) =>
          val xrefs = NonEmptyList(h, t)
          val trailers = xrefs.map(_.trailer)
          Pull.output1(Decoded.Meta(xrefs, Trailer.sanitize(trailers), version))
        case State(Nil, _) =>
          StreamUtil.failPull("no xref in TopLevel stream")
        case State(_, None) =>
          StreamUtil.failPull("no version in TopLevel stream")
      }

  def decodeTopLevelPipe: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  def decoded(log: Log): Pipe[IO, BitVector, Decoded] =
    TopLevel.pipe
      .andThen(FilterDuplicates.pipe(log))
      .andThen(decodeTopLevelPipe)
}
