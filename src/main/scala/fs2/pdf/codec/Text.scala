package fs2
package pdf
package codec

import java.nio.charset.StandardCharsets

import scala.util.Try

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._

object Text
{
  def latin: Codec[String] =
    string(StandardCharsets.ISO_8859_1)

  def digitRange: (Int, Int) =
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

  def textLenient: Codec[String] =
    withDefault(Codecs.opt(utf8), latin)

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

  def line(desc: String): Codec[ByteVector] =
    Codecs.decodeUntilAny(desc)(bytes)(Newline.newlines)

  def char(data: Char): Codec[Unit] =
    Codecs.byte(data.toByte)

  def str(data: String): Codec[Unit] =
    constant(ByteVector(data.getBytes)).withContext(s"constant string `$data`")

  def takeCharsUntilAny(chars: List[Char])(bits: BitVector): Attempt[DecodeResult[String]] = {
    val result = bits.bytes.takeWhile(a => !chars.contains(a)).bits
    latin.decode(result)
      .map { case DecodeResult(s, _) => DecodeResult(s, bits.drop(result.size)) }
  }

  def charsNoneOf(chars: List[Char]): Codec[String] =
    Codec(utf8, Decoder(takeCharsUntilAny(chars) _))

  def stringOf(count: Int): Codec[String] =
    bytes(count)
      .exmap(
        a => Attempt.fromEither(a.decodeUtf8.leftMap(a => Err(a.toString))),
        a => Attempt.successful(ByteVector(a.getBytes)),
      )
}
