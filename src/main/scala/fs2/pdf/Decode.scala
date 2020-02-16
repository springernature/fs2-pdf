package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt
import scodec.bits.BitVector

/**
  * Abstract representation of basic components of a PDF file.
  *
  * Objects come in two syntactic variants: either as top level indirect objects, or contained in an indirect object's
  * stream. This type unifies the two concepts, but splits them in those with streams ([[ContentObj]]) and those without
  * ([[DataObj]]).
  * The [[Meta]] variant carries all non-object information.
  * [[ContentObj]]s additionally provide lazily evaluated uncompressed versions of the object's stream.
  */
sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, rawStream: BitVector, stream: Uncompressed)
  extends Decoded

  case class Meta(xrefs: NonEmptyList[Xref], trailer: Trailer, version: Version)
  extends Decoded

  /**
    * Trivially turn [[Decoded]] back into [[IndirectObj]]s.
    *
    * @return [[Pipe]] that turns [[Decoded]]s into [[IndirectObj]]
    */
  def objects: Pipe[IO, Decoded, IndirectObj] =
    _.flatMap {
      case Decoded.DataObj(obj) =>
        Stream(IndirectObj(obj, None))
      case Decoded.ContentObj(obj, _, stream) =>
        StreamUtil.attemptStream("Decode.indirectObjs")(stream.exec)
          .map(Some(_))
          .map(IndirectObj(obj, _))
      case Decoded.Meta(_, _, _) =>
        fs2.Stream.empty
    }

  /**
    * State updating function for [[Rewrite.simpleParts]] that only collects the trailer and generalizes objects back
    * to [[IndirectObj]].
    *
    * @return parts and updated state
    */
  def part: RewriteState[Unit] => Decoded => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Decoded.Meta(_, trailer, _) =>
        (Nil, state.copy(trailer = Some(trailer)))
      case Decoded.DataObj(obj) =>
        (List(Part.Obj(IndirectObj(obj, None))), state)
      case Decoded.ContentObj(obj, rawStream, _) =>
        (List(Part.Obj(IndirectObj(obj, Some(rawStream)))), state)
      case _ =>
        (Nil, state)
    }

  /**
    * Trivially turn [[Decoded]] back into encodable [[Part]]s.
    *
    * @return [[Pipe]] that turns [[Decoded]]s into [[Part]]
    */
  def parts: Pipe[IO, Decoded, Part[Trailer]] =
    Rewrite.simpleParts(())(part)(update => Part.Meta(update.trailer))
}

object Decode
{
  private[this]
  case class State(xrefs: List[Xref], version: Option[Version])

  private[this]
  def decodeObjectStream[A](stream: Uncompressed)(data: Prim)
  : Option[Attempt[Either[A, List[Decoded]]]] =
    Content.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(a => Right(a.map(Decoded.DataObj(_)))))

  private[this]
  def trailer(data: Prim.Dict): Attempt[Trailer] =
    Trailer.fromData(data)

  private[this]
  def extractMetadata(stream: Uncompressed)
  : Prim => Option[Attempt[Either[Xref, List[Decoded]]]] = {
    case Prim.tpe("XRef", data) =>
      Some(stream.exec.flatMap(XrefStream(data)).map(xs => Left(Xref(xs.tables, xs.trailer, 0))))
    case _ =>
      None
  }

  private[this]
  def analyzeStream
  (index: Obj.Index, data: Prim)
  (rawStream: BitVector, stream: Uncompressed)
  : Attempt[Either[Xref, List[Decoded]]] =
    decodeObjectStream(stream)(data)
      .orElse(extractMetadata(stream)(data))
      .getOrElse(Attempt.successful(Right(List(Decoded.ContentObj(Obj(index, data), rawStream, stream)))))

  private[this]
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

  private[this]
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

  private[this]
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

  /**
    * Lazily uncompress streams, extract objects from object streams, accumulate xrefs and separate content objects from
    * data objects (those without streams).
    *
    * @return [[Pipe]] that turns [[TopLevel]]s into [[Decoded]]
    */
  def decodeTopLevel: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  /**
    * This pipe chains [[TopLevel]] with [[FilterDuplicates]], which compensates for badly encoded PDFs, and turns the
    * result into [[Decoded]].
    *
    * @param log
    * @return [[Pipe]] that turns [[BitVector]]s into [[Decoded]]
    */
  def decoded(log: Log): Pipe[IO, BitVector, Decoded] =
    TopLevel.pipe
      .andThen(FilterDuplicates.pipe(log))
      .andThen(decodeTopLevel)
}
