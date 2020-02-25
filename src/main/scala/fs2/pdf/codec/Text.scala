package fs2
package pdf
package codec

import java.nio.charset.StandardCharsets

import scala.util.Try

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

private[pdf]
object Text
{
  val latin: Codec[String] =
    string(StandardCharsets.ISO_8859_1)

  val digitRange: (Int, Int) =
    ('0', '9')

  def byteInRange(byte: Byte): (Int, Int) => Boolean =
    (low, high) =>
      byte >= low && byte <= high

  def isDigit(byte: Byte): Boolean =
    byteInRange(byte)(digitRange)

  def rangesDecoder(rs: (Int, Int)*): Decoder[ByteVector] =
    Decoder { bits =>
      val s = bits.bytes.takeWhile(a => rs.exists(byteInRange(a)))
      Attempt.successful(DecodeResult(s, bits.bytes.drop(s.size).bits))
    }

  def ranges(rs: (Int, Int)*): Codec[ByteVector] =
    Codec(bytes, rangesDecoder(rs: _*))

  def range(low: Int, high: Int): Codec[ByteVector] =
    ranges((low, high))

  private[pdf]
  def digitsDecoder: Decoder[String] =
    range('0', '9').map(a => new String(a.toArray))

  object ascii
  {
    def digits: Codec[String] =
      Codec(scodec.codecs.ascii, digitsDecoder)

    def digits1: Codec[String] =
      digits.exmap(
        a => if (a.isEmpty) Scodec.fail("input does not start with a digit") else Attempt.successful(a),
        Attempt.successful,
      )

    def long: Codec[Long] =
      digits1.exmap(a => Attempt.fromTry(Try(a.toLong)), a => Attempt.successful(a.toString))

    def int: Codec[Int] =
      digits1.exmap(a => Attempt.fromTry(Try(a.toInt)), a => Attempt.successful(a.toString))
  }

  def sanitizeNewlines(in: String): String =
    in
      .replaceAll("[\r\n]+", "<<NL>>")
      .replaceAll(" ", "<<SPACE>>")
      .replaceAll("\\p{C}", "?")
      .replaceAll("<<NL>>", "\n")
      .replaceAll("<<SPACE>>", " ")

  def sanitizedLatin: Codec[String] =
    latin.xmap(sanitizeNewlines, identity)

  def sanitize(data: ByteVector): String =
    sanitizedLatin.decode(data.bits).map(_.value).getOrElse("unparsable")

  def sanitizeBits(data: BitVector): String =
    sanitize(data.bytes)

  private[pdf]
  val lineDecoder: Decoder[ByteVector] =
    Decoder { bits =>
      val result = bits.bytes.takeWhile(a => !Newline.isNewlineByte(a))
      Attempt.successful(DecodeResult(result, bits.bytes.drop(result.size).bits))
    }

  def line(desc: String): Codec[ByteVector] =
    (Codec(bytes, lineDecoder) <~ Newline.newline)
      .withContext(desc)

  def char(data: Char): Codec[Unit] =
    Codecs.byte(data.toByte)

  def str(data: String): Codec[Unit] =
    constant(ByteVector(data.getBytes)).withContext(s"constant string `$data`")

  private[pdf]
  def takeCharsUntilAny(decoder: Codec[String])(chars: List[Char])(bits: BitVector): Attempt[DecodeResult[String]] = {
    val result = bits.bytes.takeWhile(a => !chars.contains(a)).bits
    decoder.decode(result)
      .map { case DecodeResult(s, _) => DecodeResult(s, bits.drop(result.size)) }
  }

  def charsNoneOf(decoder: Codec[String])(chars: List[Char]): Codec[String] =
    Codec(utf8, Decoder(takeCharsUntilAny(decoder)(chars) _))

  def stringOf(count: Int): Codec[String] =
    bytes(count)
      .exmap(
        a => Attempt.fromEither(a.decodeUtf8.leftMap(a => Err(a.toString))),
        a => Attempt.successful(ByteVector(a.getBytes)),
      )
}
