package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}
import scodec.{Attempt, Codec, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}
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

  case class Meta(xrefs: NonEmptyList[Xref], trailer: Trailer, version: Version)
  extends Decoded
}

object Content
{
  def extractObjectStream(stream: Eval[Attempt[BitVector]]): Prim => Option[Attempt[ObjectStream]] = {
    case Prim.tpe("ObjStm", _) =>
      Some(
        stream
          .value
          .flatMap(ObjectStream.Codec_ObjectStream.complete.decode)
          .map(_.value)
      )
    case _ =>
      None
  }

  def uncompress(stream: BitVector)
  : Prim => Eval[Attempt[BitVector]] = {
    case Prim.filter("FlateDecode", data) =>
      Eval.later(FlateDecode(stream, data))
    case _ =>
      Eval.now(Attempt.successful(stream))
  }

  def streamLength(dict: Prim): Attempt[Long] =
    Prim.Dict.number("Length")(dict).map(_.toLong)

  val streamEndMarker: ByteVector =
    ByteVector("endstream".getBytes)

  def endstreamIndex(bytes: ByteVector): Attempt[Long] =
    bytes.indexOfSlice(streamEndMarker) match {
      case i if i >= 0 => Attempt.successful(i)
      case _ => Attempt.failure(Err.InsufficientBits(0, bytes.bits.size, List("no stream end position found")))
    }
}

object Decode
{
  case class State(xrefs: List[Xref], version: Option[Version])

  def decodeObjectStream[A](stream: Eval[Attempt[BitVector]])(data: Prim)
  : Option[Attempt[Either[A, List[Decoded]]]] =
    Content.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(a => Right(a.map(Decoded.DataObj(_)))))

  def trailer(data: Prim.Dict): Attempt[Trailer] =
    Attempt.fromOption(
      Prim.path("Size")(data) { case Prim.Number(size) => Trailer(size, data) },
      Err("no Size in xref stream data"),
    )

  def extractMetadata(stream: Eval[Attempt[BitVector]])
  : Prim => Option[Attempt[Either[Xref, List[Decoded]]]] = {
    case Prim.tpe("XRef", data) =>
      Some(stream.value.flatMap(XrefStream(data)).map(xs => Left(Xref(xs.tables, xs.trailer, 0))))
    case _ =>
      None
  }

  def analyzeStream
  (index: Obj.Index, data: Prim)
  (rawStream: BitVector, stream: Eval[Attempt[BitVector]])
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
        case _ =>
          StreamUtil.failPull("no xref or version")
      }

  def decodeTopLevelPipe: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  def decoded(log: Log): Pipe[IO, BitVector, Decoded] =
    TopLevel.pipe
      .andThen(FilterDuplicates.pipe(log))
      .andThen(decodeTopLevelPipe)
}
