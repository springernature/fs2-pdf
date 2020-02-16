package fs2
package pdf
package codec

import cats.data.NonEmptyList
import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.BitVector
import scodec.codecs._

object Many
{
  /**
    * This function is consulted by listMultiplexed before trying to decode an element.
    * Here we want to decode as many elements as possible until the 'end' condition succeeds, so we just return the
    * whole vector if 'end' fails and an empty vector otherwise.
    *
    * @param in remaining input
    * @return input for next element, remainder for later iterations
    */
  def demuxTill(end: BitVector => Boolean)(in: BitVector): (BitVector, BitVector) =
    if (in.isEmpty || end(in)) (BitVector.empty, in) else (in, BitVector.empty)

  /**
    * Decode as many elements of type A as possible, until the remaining input vector satisfies the condition 'end' or
    * is exhausted.
    * The end condition is optional if the input is exhausted, so if you want to enforce the end condition, use
    * something like 'Many.tillDecodes(end)(main) <~ end'.
    *
    * @param end termination condition
    * @param main element codec
    */
  def till[A](end: BitVector => Boolean)(main: Codec[A]): Codec[List[A]] =
    listMultiplexed(_ ++ _, demuxTill(end), main).withContext("manyTill")

  def tillDecodes[A](end: Codec[Unit]): Codec[A] => Codec[List[A]] =
    till(end.decode(_).isSuccessful)

  def till1[A](end: BitVector => Boolean)(main: Codec[A]): Codec[NonEmptyList[A]] =
    till(end)(main)
      .exmap(Scodec.attemptNel("till1"), a => Attempt.successful(a.toList))

  def tillDecodes1[A](end: Codec[Unit]): Codec[A] => Codec[NonEmptyList[A]] =
    till1(end.decode(_).isSuccessful)

  def bracket[A](start: Codec[Unit], end: Codec[Unit])(main: Codec[A]): Codec[List[A]] =
    Codecs.bracket(start, end)(tillDecodes(end)(main))

  private[this]
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

  def apply[A](indicator: Codec[Unit], target: Codec[A]): Codec[List[A]] =
    Codec(list(target), manyDecoder(indicator, target))
}
