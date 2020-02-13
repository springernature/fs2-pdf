package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}
import scodec.{Attempt, Codec, Decoder, Err}
import scodec.bits.BitVector
import scodec.stream.StreamDecoder
import shapeless.{:+:, CNil}

sealed trait TopLevel

object TopLevel
{
  case class IndirectObj(obj: pdf.IndirectObj)
  extends TopLevel

  case class Version(version: pdf.Version)
  extends TopLevel

  case class Comment(data: pdf.Comment)
  extends TopLevel

  case class Xref(version: pdf.Xref)
  extends TopLevel

  case class StartXref(startxref: pdf.StartXref)
  extends TopLevel

  /**
    * Decode a top level PDF element like indirect objects, version tags, comments and cross reference tables.
    * The coproduct type must be specified explicitly because the macro will order the types alphabetically, making
    * Comment supersede Version.
    *
    * @return [[Decoder]] for [[TopLevel]]
    */
  def Decoder_TopLevel: Decoder[TopLevel] =
    Codec.coproduct[IndirectObj :+: Version :+: Comment :+: Xref :+: StartXref :+: CNil].choice.as[TopLevel]

  /**
    * Decode a top level PDF element like indirect objects, version tags, comments and cross reference tables.
    * Wrapped decoder for the use in streams.
    * Since [[Decoder_TopLevel]] is a choice decoder, it will return an error that causes
    * [[StreamDecoder]] to terminate, so we force the error to be [[Err.InsufficientBits]].
    *
    * @return [[Decoder]] for [[TopLevel]]
    */
  def streamDecoder: Decoder[TopLevel] =
    Decoder(bits => Decoder_TopLevel.decode(bits).mapErr(e => Err.InsufficientBits(0, 0, e.context)))

  def pipe: Pipe[IO, BitVector, TopLevel] =
    StreamDecoder.many(streamDecoder).toPipe
}

sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, rawStream: BitVector, stream: Eval[Attempt[BitVector]])
  extends Decoded

  case class Meta(trailer: Trailer, version: Version)
  extends Decoded
}

object Decode
{
  case class State(trailers: List[Trailer], version: Option[Version])

  def decodeObjectStream(stream: Eval[Attempt[BitVector]])(data: Prim)
  : Option[Attempt[Either[Trailer, List[Decoded]]]] =
    ParseObjects.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(a => Right(a.map(Decoded.DataObj(_)))))

  def trailer(data: Prim.Dict): Attempt[Trailer] =
    Attempt.fromOption(
      Prim.path("Size")(data) { case Prim.Number(size) => Trailer(size, data) },
      Err("no Size in xref stream data"),
    )

  def extractMetadata: Prim => Option[Attempt[Either[Trailer, List[Decoded]]]] = {
    case Prim.tpe("XRef", data) =>
      Some(trailer(data).map(Left(_)))
    case _ =>
      None
  }

  def analyzeStream
  (index: Obj.Index, data: Prim)
  (rawStream: BitVector, stream: Eval[Attempt[BitVector]])
  : Attempt[Either[Trailer, List[Decoded]]] =
    decodeObjectStream(stream)(data)
      .orElse(extractMetadata(data))
      .getOrElse(Attempt.successful(Right(List(Decoded.ContentObj(Obj(index, data), rawStream, stream)))))

  def contentObj
  (state: State)
  (index: Obj.Index, data: Prim, stream: BitVector)
  : Pull[IO, Decoded, State] =
    StreamUtil.attemptPullWith("extract stream objects")(
      analyzeStream(index, data)(stream, ParseObjects.uncompressStrippedStream(stream)(data))
    ) {
      case Right(decoded) => Pull.output(Chunk.seq(decoded)).as(state)
      case Left(trailer) => Pull.pure(state.copy(trailers = trailer :: state.trailers))
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
      Pull.pure(state.copy(trailers = xref.trailer :: state.trailers))
    case TopLevel.StartXref(_) =>
      Pull.pure(state)
    case TopLevel.Comment(_) =>
      Pull.pure(state)
  }

  def decodeTopLevelPull(in: Stream[IO, TopLevel]): Pull[IO, Decoded, Unit] =
    StreamUtil.pullState(pullTopLevel)(in)(State(Nil, None))
      .flatMap {
        case State(h :: t, Some(version)) =>
          Pull.output1(Decoded.Meta(Trailer.sanitize(NonEmptyList(h, t)), version))
        case _ =>
          StreamUtil.failPull("no xref or version")
      }

  def decodeTopLevelPipe: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  def decoded(log: Log): Pipe[IO, BitVector, Decoded] =
    TopLevel.pipe
      .andThen(FilterDuplicatesTopLevel.pipe(log))
      .andThen(decodeTopLevelPipe)
}
