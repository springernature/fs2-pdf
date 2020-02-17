package fs2
package pdf
package codec

import cats.data.NonEmptyList
import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

private[pdf]
object Codecs
{
  def byte(data: Byte): Codec[Unit] =
    constant(ByteVector.fromByte(data))

  def bracket[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[A] =
    start ~> main <~ end

  def bracketChar[A](start: Char, end: Char): Codec[A] => Codec[A] =
    bracket(Text.char(start), Text.char(end))

  def encode[A](a: A)(implicit encoder: Encoder[A]): Attempt[BitVector] =
    encoder.encode(a)

  def encodeBytes[A: Encoder](a: A): Attempt[ByteVector] =
    encode(a).map(_.bytes)

  def encodeOpt[A: Encoder]: Encoder[Option[A]] =
    Encoder {
      case Some(a) => Encoder[A].encode(a)
      case None => Attempt.successful(BitVector.empty)
    }

  def decodeOpt[A: Decoder]: Decoder[Option[A]] =
    Decoder { bits =>
      Decoder[A].decode(bits) match {
        case Attempt.Successful(DecodeResult(a, rm)) =>
          Attempt.Successful(DecodeResult(Some(a), rm))
        case _ =>
          Attempt.Successful(DecodeResult(None, bits))
      }
    }

  def opt[A: Codec]: Codec[Option[A]] =
    Codec(encodeOpt, decodeOpt)

  def nelOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[NonEmptyList[A]] =
    listOfN(countCodec, valueCodec)
      .exmap(Scodec.attemptNel("nelOfN"), a => Attempt.successful(a.toList))
}
