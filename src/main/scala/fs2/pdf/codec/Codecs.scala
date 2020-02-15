package fs2
package pdf
package codec

import cats.data.NonEmptyList
import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs._
import shapeless.{Generic, HList}

object Codecs
{
  def byte(data: Byte): Codec[Unit] =
    constant(ByteVector.fromByte(data))

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

  /**
    * This function is consulted by listMultiplexed before trying to decode an element.
    * Here we want to decode as many elements as possible until the 'end' condition succeeds, so we just return the
    * whole vector if 'end' fails and an empty vector otherwise.
    *
    * @param in remaining input
    * @return input for next element, remainder for later iterations
    */
  def demuxManyTill(end: BitVector => Boolean)(in: BitVector): (BitVector, BitVector) =
    if (in.isEmpty || end(in)) (BitVector.empty, in) else (in, BitVector.empty)

  /**
    * Decode as many elements of type A as possible, until the remaining input vector satisfies the condition 'end' or
    * is exhausted.
    * The end condition is optional if the input is exhausted, so if you want to enforce the end condition, use
    * something like 'manyTillCodec(end)(main) <~ end'.
    *
    * @param end termination condition
    * @param main element codec
    */
  def manyTill[A](end: BitVector => Boolean)(main: Codec[A]): Codec[List[A]] =
    listMultiplexed(_ ++ _, demuxManyTill(end), main).withContext("manyTill")

  def manyTillCodec[A](end: Codec[Unit]): Codec[A] => Codec[List[A]] =
    manyTill(end.decode(_).isSuccessful)

  def manyTill1[A](end: BitVector => Boolean)(main: Codec[A]): Codec[NonEmptyList[A]] =
    manyTill(end)(main)
      .exmap(Scodec.attemptNel("manyTill1"), a => Attempt.successful(a.toList))

  def manyTill1Codec[A](end: Codec[Unit]): Codec[A] => Codec[NonEmptyList[A]] =
    manyTill1(end.decode(_).isSuccessful)

  def bracket[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[A] =
    start ~> main <~ end

  def bracketMany[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[List[A]] =
    bracket(start, end)(manyTillCodec(end)(main))

  def bracketChar[A](start: Char, end: Char): Codec[A] => Codec[A] =
    bracket(Text.char(start), Text.char(end))

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

  def manyDecoder[A](indicator: Codec[Unit], target: Codec[A]): Decoder[List[A]] = {
    def one: Decoder[Option[A]] =
      optional(recover(indicator), target)
    @annotation.tailrec
    def spin(acc: List[A])(bits: BitVector): Attempt[DecodeResult[List[A]]] =
      one.decode(bits) match {
        case Attempt.Successful(DecodeResult(None, remainder)) =>
          Attempt.successful(DecodeResult(acc, remainder))
        case Attempt.Successful(DecodeResult(Some(a), remainder)) =>
          spin(a :: acc)(remainder)
        case Attempt.Failure(cause) =>
          Attempt.Failure(cause)
      }
    Decoder(spin(Nil) _).map(_.reverse)
  }

  def many[A](indicator: Codec[Unit], target: Codec[A]): Codec[List[A]] =
    Codec(list(target), manyDecoder(indicator, target))

  def nelOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[NonEmptyList[A]] =
    listOfN(countCodec, valueCodec)
      .exmap(Scodec.attemptNel("nelOfN"), a => Attempt.successful(a.toList))
}
