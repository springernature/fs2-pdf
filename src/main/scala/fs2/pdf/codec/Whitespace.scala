package fs2
package pdf
package codec

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder}
import scodec.bits.ByteVector
import scodec.codecs._
import scodec.interop.cats.DecoderMonadInstance

private[pdf]
object Whitespace
{
  import Newline._

  val whitespaceBytes: Decoder[ByteVector] =
    Decoder.choiceDecoder(
      constant(lfBytes).map(_ => lfBytes),
      constant(crlfBytes).map(_ => crlfBytes),
      constant(crBytes).map(_ => crBytes),
      constant(spaceBytes).map(_ => spaceBytes),
      constant(tabBytes).map(_ => tabBytes),
    )

  val multiWhitespaceDecoder: Decoder[ByteVector] =
    Decoder { bits =>
      val bytes = bits.bytes
      val ws = bytes.takeWhile(_.toChar.isWhitespace)
      Attempt.successful(DecodeResult(ws, bytes.drop(ws.size).bits))
    }

  def multiWhitespace(encode: Encoder[Unit]): Codec[Unit] =
    Codec(encode, multiWhitespaceDecoder.void)

  def multiWhitespaceByte(b: Byte): Codec[Unit] =
    multiWhitespace(Codecs.byte(b))

  val whitespace: Codec[Unit] =
    choice(
      constant(lfBytes),
      constant(crlfBytes),
      constant(crBytes),
      constant(spaceBytes),
      constant(tabBytes),
    )

  val ws: Codec[Unit] =
    multiWhitespaceByte(spaceByte)

  val skipWs: Codec[Unit] =
    multiWhitespace(provide(()))

  val whitespaceAsNewline: Codec[Unit] =
    multiWhitespaceByte(lfByte)

  val whitespaceAndCommentAsNewline: Codec[Unit] =
    skipWs ~> Comment.many.unit(Nil) ~> whitespaceAsNewline

  val nlWs: Codec[Unit] =
    whitespaceAndCommentAsNewline

  val space: Codec[Unit] =
    Codecs.byte(' ')
}
