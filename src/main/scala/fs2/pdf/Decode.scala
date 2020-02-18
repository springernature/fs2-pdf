package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt
import scodec.bits.BitVector

/**
  * Takes care of converting [[TopLevel]] into [[Decoded]], splitting PDF objects into [[DataObj]] and [[ContentObj]],
  * discriminated by the existence of a content stream, and aggregating xref and version information.
  */
object Decode
{
  private[pdf]
  case class State(xrefs: List[Xref], version: Option[Version])

  private[pdf]
  def decodeObjectStream[A](stream: Uncompressed)(data: Prim)
  : Option[Attempt[Either[A, List[Decoded]]]] =
    Content.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(a => Right(a.map(Decoded.DataObj(_)))))

  private[pdf]
  def trailer(data: Prim.Dict): Attempt[Trailer] =
    Trailer.fromData(data)

  private[pdf]
  def extractMetadata(stream: Uncompressed)
  : Prim => Option[Attempt[Either[Xref, List[Decoded]]]] = {
    case Prim.tpe("XRef", data) =>
      Some(stream.exec.flatMap(XrefStream(data)).map(xs => Left(Xref(xs.tables, xs.trailer, 0))))
    case _ =>
      None
  }

  private[pdf]
  def analyzeStream
  (index: Obj.Index, data: Prim)
  (rawStream: BitVector, stream: Uncompressed)
  : Attempt[Either[Xref, List[Decoded]]] =
    decodeObjectStream(stream)(data)
      .orElse(extractMetadata(stream)(data))
      .getOrElse(Attempt.successful(Right(List(Decoded.ContentObj(Obj(index, data), rawStream, stream)))))

  private[pdf]
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

  private[pdf]
  def pullTopLevel(state: State): TopLevel => Pull[IO, Decoded, State] = {
    case TopLevel.IndirectObj(IndirectObj(Obj(index, data), Some(stream))) =>
      contentObj(state)(index, data, stream)
    case TopLevel.IndirectObj(IndirectObj(Obj(_, Prim.Dict(data)), None)) if data.contains("Linearized") =>
      Pull.pure(state)
    case TopLevel.IndirectObj(IndirectObj(obj, None)) =>
      Pull.output1(Decoded.DataObj(obj)).as(state)
    case TopLevel.Version(version) =>
      Pull.pure(state.copy(version = Some(version)))
    case TopLevel.Xref(xref) =>
      Pull.pure(state.copy(xrefs = xref :: state.xrefs))
    case TopLevel.StartXref(_) =>
      Pull.pure(state)
  }

  private[pdf]
  def decodeTopLevelPull(in: Stream[IO, TopLevel]): Pull[IO, Decoded, Unit] =
    StreamUtil.pullState(pullTopLevel)(in)(State(Nil, None))
      .flatMap {
        case State(xrefs, version) =>
          val trailers = xrefs.map(_.trailer)
          Pull.output1(Decoded.Meta(xrefs, NonEmptyList.fromList(trailers).map(Trailer.sanitize), version))
      }

  /**
    * Lazily uncompress streams, extract objects from object streams, accumulate xrefs and separate content objects from
    * data objects (those without streams).
    *
    * @return [[Pipe]] that turns [[TopLevel]]s into [[Decoded]]
    */
  def fromTopLevel: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  /**
    * This pipe chains [[TopLevel]] with [[FilterDuplicates]], which compensates for badly encoded PDFs, and turns the
    * result into [[Decoded]].
    *
    * @param log
    * @return [[Pipe]] that turns [[BitVector]]s into [[Decoded]]
    */
  def apply(log: Log): Pipe[IO, BitVector, Decoded] =
    TopLevel.pipe
      .andThen(FilterDuplicates.pipe(log))
      .andThen(fromTopLevel)
}
