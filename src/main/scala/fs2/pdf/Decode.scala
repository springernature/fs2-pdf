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

  /**
    * Decode a top level PDF object like indirect objects, version tags, comments and cross reference tables.
    * The coproduct type must be specified explicitly because the macro will order the types alphabetically, making
    * Comment supersede Version.
    *
    * @return [[Decoder]] for [[TopLevel]]
    */
  def Decoder_TopLevel: Decoder[TopLevel] =
    Codec.coproduct[IndirectObj :+: Version :+: Comment :+: Xref :+: CNil].choice.as[TopLevel]

  /**
    * Decode a top level PDF object like indirect objects, version tags, comments and cross reference tables.
    * Wrapped decoder for the use in streams.
    * Since [[Decoder_TopLevel]] is a choice decoder, it will return an error that causes
    * [[StreamDecoder]] to terminate, so we force the error to be [[Err.InsufficientBits]].
    *
    * @return [[Decoder]] for [[TopLevel]]
    */
  def streamDecoder: Decoder[TopLevel] =
    Decoder(bits => Decoder_TopLevel.decode(bits).mapErr(e => Err.InsufficientBits(0, 0, e.context)))

  def pipe: Pipe[IO, Byte, TopLevel] =
    StreamDecoder.many(streamDecoder).toPipeByte
}

sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, stream: Eval[Attempt[BitVector]])
  extends Decoded

  case class Meta(xrefs: NonEmptyList[Xref], version: Version)
  extends Decoded
}

object Decode
{
  case class State(xrefs: List[Xref], version: Option[Version])

  def decodeObjectStream(stream: Eval[Attempt[BitVector]])(data: Prim): Option[Attempt[List[Decoded]]] =
    ParseObjects.extractObjectStream(stream)(data)
      .map(_.map(_.objs).map(_.map(Decoded.DataObj(_))))

  def analyzeStream(index: Obj.Index, data: Prim)(stream: Eval[Attempt[BitVector]]): Attempt[List[Decoded]] =
    decodeObjectStream(stream)(data)
      .getOrElse(Attempt.successful(List(Decoded.ContentObj(Obj(index, data), stream))))

  def contentObj(index: Obj.Index, data: Prim, stream: BitVector): Pull[IO, Decoded, Unit] =
    StreamUtil.attemptPullWith("extract stream objects")(
      analyzeStream(index, data)(ParseObjects.uncompressStrippedStream(stream)(data))
    )(a => Pull.output(Chunk.seq(a)))

  def pullTopLevel(state: State): TopLevel => Pull[IO, Decoded, State] = {
    case TopLevel.IndirectObj(IndirectObj(index, data, Some(stream))) =>
      contentObj(index, data, stream).as(state)
    case TopLevel.IndirectObj(IndirectObj(index, data, None)) =>
      Pull.output1(Decoded.DataObj(Obj(index, data))).as(state)
    case TopLevel.Version(version) =>
      Pull.pure(state.copy(version = Some(version)))
    case TopLevel.Xref(xref) =>
      Pull.pure(state.copy(xrefs = xref :: state.xrefs))
    case TopLevel.Comment(_) =>
      Pull.pure(state)
  }

  def decodeTopLevelPull(in: Stream[IO, TopLevel]): Pull[IO, Decoded, Unit] =
    StreamUtil.pullState(pullTopLevel)(in)(State(Nil, None))
      .flatMap {
        case State(h :: t, Some(version)) =>
          Pull.output1(Decoded.Meta(NonEmptyList(h, t), version))
        case _ =>
          StreamUtil.failPull("no xref or version")
      }

  def decodeTopLevelPipe: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  def decoded: Pipe[IO, Byte, Decoded] =
    TopLevel.pipe.andThen(decodeTopLevelPipe)
}
