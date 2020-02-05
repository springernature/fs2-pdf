package fs2
package pdf

import java.nio.charset.StandardCharsets

import scala.util.Try

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}
import scodec.bits.{BitVector, ByteVector, HexStringSyntax}
import scodec.codecs._
import scodec.interop.cats.DecoderMonadInstance
import shapeless.{Generic, HList}

object Codecs
{
  val newlineChar: Char =
    '\n'

  val newlineByte: Byte =
    newlineChar.toByte

  val newlineByteVector: ByteVector =
    ByteVector.fromByte(newlineByte)

  val linuxNewlineBytes: ByteVector =
    hex"0A"

  val linuxNewlineByte: Byte =
    linuxNewlineBytes.toByte()

  val windowsNewlineBytes: ByteVector =
    hex"0D0A"

  val macosNewlineBytes: ByteVector =
    hex"0D"

  val macosNewlineByte: Byte =
    macosNewlineBytes.toByte()

  val spaceBytes: ByteVector =
    hex"20"

  val tabBytes: ByteVector =
    hex"0B"

  val newlines: List[ByteVector] =
    List(linuxNewlineBytes, windowsNewlineBytes, macosNewlineBytes)

  val linuxNewline: Codec[Unit] =
    constant(linuxNewlineBytes)

  val windowsNewline: Codec[Unit] =
    constant(windowsNewlineBytes)

  val macosNewline: Codec[Unit] =
    constant(macosNewlineBytes)

  val newline: Codec[Unit] =
    choice(constant(linuxNewlineBytes), constant(windowsNewlineBytes), constant(macosNewlineBytes))

  val newlineBytes: Decoder[ByteVector] =
    Decoder.choiceDecoder(
      constant(linuxNewlineBytes).map(_ => linuxNewlineBytes),
      constant(windowsNewlineBytes).map(_ => windowsNewlineBytes),
      constant(macosNewlineBytes).map(_ => macosNewlineBytes),
    )

  val newlineBits: Decoder[BitVector] =
    Decoder.choiceDecoder(
      constant(linuxNewlineBytes).map(_ => linuxNewlineBytes.bits),
      constant(windowsNewlineBytes).map(_ => windowsNewlineBytes.bits),
      constant(macosNewlineBytes).map(_ => macosNewlineBytes.bits),
    )

  def stripNewline(bytes: ByteVector): ByteVector =
    if (bytes.takeRight(2) == Codecs.windowsNewlineBytes) bytes.dropRight(2)
    else if (bytes.takeRight(1) == Codecs.linuxNewlineBytes) bytes.dropRight(1)
    else if (bytes.takeRight(1) == Codecs.macosNewlineBytes) bytes.dropRight(1)
    else bytes

  val whitespaceBytes: Decoder[ByteVector] =
    Decoder.choiceDecoder(
      constant(linuxNewlineBytes).map(_ => linuxNewlineBytes),
      constant(windowsNewlineBytes).map(_ => windowsNewlineBytes),
      constant(macosNewlineBytes).map(_ => macosNewlineBytes),
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

  def multiWhitespaceChar(c: Char): Codec[Unit] =
    multiWhitespace(char(c))

  val whitespace: Codec[Unit] =
    choice(
      constant(linuxNewlineBytes),
      constant(windowsNewlineBytes),
      constant(macosNewlineBytes),
      constant(spaceBytes),
      constant(tabBytes),
    )

  val ws: Codec[Unit] =
    multiWhitespaceChar(' ')

  val nlWs: Codec[Unit] =
    multiWhitespaceChar(newlineChar)

  val skipWs: Codec[Unit] =
    multiWhitespace(provide(()))

  def partialBytes[A]
  (desc: String)
  (take: ByteVector => Option[ByteVector])
  (f: ByteVector => Attempt[DecodeResult[A]])
  (bits: BitVector)
  : Attempt[DecodeResult[A]] = {
    take(bits.bytes) match {
      case Some(prefix) =>
        f(prefix).flatMap {
          case DecodeResult(a, remainder) if remainder.isEmpty =>
            Attempt.Successful(DecodeResult(a, bits.bytes.drop(prefix.size).bits))
          case _ =>
            Attempt.failure(Err(s"partial decoder `$desc` consumed too few bytes"))
        }
      case None =>
        Attempt.failure(Err.insufficientBits(0, 0))
    }
  }

  def takeBytesUntil(delimiter: Byte)(input: ByteVector): Option[ByteVector] = {
    val result = input.takeWhile(_ != delimiter)
    if (result.length == input.length) None
    else Some(result)
  }

  def takeBytesAny(delimiters: List[ByteVector])(input: ByteVector): Option[ByteVector] = {
    val next = delimiters
      .map(input.indexOfSlice)
      .filter(_ >= 0)
      .minOption
    val result = next.map(input.take).getOrElse(input)
    if (result.length == input.length) None
    else Some(result)
  }

  def parseBytesUntil[A]
  (desc: String)
  (delimiter: Byte)
  : (ByteVector => Attempt[DecodeResult[A]]) => BitVector => Attempt[DecodeResult[A]] =
    partialBytes[A](desc)(takeBytesUntil(delimiter))

  def parseBytesUntilAny[A]
  (desc: String)
  (delimiters: List[ByteVector])
  : (ByteVector => Attempt[DecodeResult[A]]) => BitVector => Attempt[DecodeResult[A]] =
    partialBytes[A](desc)(takeBytesAny(delimiters))

  def decodeUntilAny[A](desc: String)(bytesCodec: Codec[A])(delimiters: List[ByteVector]): Codec[A] =
    Codec(
      bytesCodec,
      Decoder(parseBytesUntilAny(desc)(delimiters)(b => bytesCodec.decode(b.bits))(_))) <~ choice(delimiters.map(constant(_)): _*
      )

  def takeBytesUntilAfter
  (end: ByteVector, followedBy: Decoder[ByteVector])
  (data: ByteVector)
  : Attempt[DecodeResult[ByteVector]] =
    data.indexOfSlice(end) match {
      case i if i > 0 =>
        val result = data.take(i + end.size)
        followedBy.decode(data.drop(i + end.size).bits).map {
          case DecodeResult(value, remainder) =>
            DecodeResult(result ++ value, remainder)
        }
      case _ =>
        Attempt.failure(Err.insufficientBits(0, 0))
    }

  def line(desc: String): Codec[ByteVector] =
    decodeUntilAny(desc)(bytes)(newlines)

  def utf8Line(desc: String): Codec[String] =
    decodeUntilAny(desc)(utf8)(newlines)

  def byte(data: Byte): Codec[Unit] =
    constant(ByteVector.fromByte(data))

  def char(data: Char): Codec[Unit] =
    byte(data.toByte)

  def constantString(data: String): Codec[Unit] =
    constant(ByteVector(data.getBytes)).withContext(s"constant string `$data`")

  def str(data: String): Codec[Unit] =
    constantString(data)

  def takeCharsUntilAny(chars: List[Char])(bits: BitVector): Attempt[DecodeResult[String]] = {
    val result = bits.bytes.takeWhile(a => !chars.contains(a)).bits
    latin.decode(result)
      .map { case DecodeResult(s, _) => DecodeResult(s, bits.drop(result.size)) }
  }

  def charsNoneOf(chars: List[Char]): Codec[String] =
    Codec(utf8, Decoder(takeCharsUntilAny(chars) _))

  def constantLine(content: String): Codec[Unit] =
    constantString(content) <~ newline

  def space: Codec[Unit] =
    byte(' ')

  def stringOf(count: Int): Codec[String] =
    bytes(count)
      .exmap(
        a => Attempt.fromEither(a.decodeUtf8.leftMap(a => Err(a.toString))),
        a => Attempt.successful(ByteVector(a.getBytes)),
      )

  /**
    * Decode as many elements of type A as possible, until the remaining input vector satisfies the condition 'end' or
    * is exhausted.
    * The end condition is optional if the input is exhausted, so if you want to enforce the end condition, use
    * something like 'manyTillCodec(end)(main) <~ end'.
    *
    * @param end termination condition
    * @param main element codec
    */
  def manyTill[A](end: BitVector => Boolean)(main: Codec[A]): Codec[List[A]] = {
    /**
      * This function is consulted by listMultiplexed before trying to decode an element.
      * Here we want to decode as many elements as possible until the 'end' condition succeeds, so we just return the
      * whole vector if 'end' fails and an empty vector otherwise.
      *
      * @param in remaining input
      * @return input for next element, remainder for later iterations
      */
    def demux(in: BitVector): (BitVector, BitVector) =
      if (in.isEmpty || end(in)) (BitVector.empty, in) else (in, BitVector.empty)
    listMultiplexed(_ ++ _, demux, main).withContext("manyTill")
  }

  def manyTillCodec[A](end: Codec[Unit]): Codec[A] => Codec[List[A]] = {
    manyTill(end.decode(_).isSuccessful)
  }

  def bracket[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[A] =
    start ~> main <~ end

  def bracketMany[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[List[A]] =
    bracket(start, end)(manyTillCodec(end)(main))

  def bracketChar[A](start: Char, end: Char): Codec[A] => Codec[A] =
    bracket(Codecs.char(start), Codecs.char(end))

  class ProductCodec[A]
  {
    def apply[P, L <: HList]
    (codec: Codec[P])
    (implicit flp: FlattenLeftPairs.Aux[P, L], gen: Generic.Aux[A, L])
    : Codec[A] =
      codec
        .flattenLeftPairs
        .xmap(gen.from, gen.to)
  }

  def productCodecFor[A]: ProductCodec[A] =
    new ProductCodec[A]

  def productCodec[A, P, L <: HList]
  (codec: Codec[P])
  (implicit flp: FlattenLeftPairs.Aux[P, L], gen: Generic.Aux[A, L])
  : Codec[A] =
    productCodecFor[A](codec)

  def productEncoder[A, P, L <: HList]
  (encoder: Encoder[P])
  (implicit flp: FlattenLeftPairs.Aux[P, L], gen: Generic.Aux[A, L])
  : Encoder[A] =
    productCodec(encoder.encodeOnly)

  def encode[A](a: A)(implicit encoder: Encoder[A]): Attempt[BitVector] =
    encoder.encode(a)

  def encodeBytes[A: Encoder](a: A): Attempt[ByteVector] =
    encode(a).map(_.bytes)

  def attemptEither[A, B](eab: Either[A, B]): Attempt[B] =
    Attempt.fromEither(eab.leftMap(a => Err(a.toString)))

  def fail[A](message: String): Attempt[A] =
    Attempt.failure(Err(message))

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
        a => if (a.isEmpty) fail("empty digits") else Attempt.successful(a),
        Attempt.successful,
      )

    def long: Codec[Long] =
      digits1.exmap(a => Attempt.fromTry(Try(a.toLong)), a => Attempt.successful(a.toString))

    def int: Codec[Int] =
      digits1.exmap(a => Attempt.fromTry(Try(a.toInt)), a => Attempt.successful(a.toString))
  }

  def textLenient: Codec[String] =
    withDefault(opt(utf8), latin)

  def sanitizeNewlines(in: String): String =
    in
      .replaceAll("[\r\n]+", "<<NL>>")
      .replaceAll(" ", "<<SPACE>>")
      .replaceAll("\\p{C}", "?")
      .replaceAll("<<NL>>", "\n")
      .replaceAll("<<SPACE>>", " ")

  def sanitizedLatin: Codec[String] =
    Codecs.latin.xmap(sanitizeNewlines, identity)

  val percent: ByteVector =
    ByteVector.fromByte('%')

  def removeComments(bytes: ByteVector): ByteVector = {
    def dropLine(input: ByteVector): ByteVector =
      line("comment").decode(input.bits) match {
        case Attempt.Successful(DecodeResult(_, remainder)) =>
          remainder.bytes
        case Attempt.Failure(_) =>
          ByteVector.empty
      }

    @annotation.tailrec
    def spin(input: ByteVector, output: ByteVector): ByteVector =
      input.indexOfSlice(percent) match {
        case -1 => output ++ input
        case i if input.lift(i + 1).contains('%') =>
          spin(input.drop(i + 2), output ++ input.take(i + 2))
        case i =>
          spin(dropLine(input.drop(i + 1)), output ++ input.take(i) ++ newlineByteVector)
      }
    spin(bytes, ByteVector.empty)
  }
}
