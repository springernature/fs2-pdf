package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe}
import scodec.{Codec, Decoder, Err}
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
