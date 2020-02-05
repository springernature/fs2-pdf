package fs2
package pdf

import cats.ApplicativeError
import cats.effect.IO
import cats.implicits._
import fs2.{Chunk, Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}

object StreamUtil
{
  def fail[F[_], A](message: String)(implicit error: ApplicativeError[F, Throwable]): F[A] =
    error.raiseError(new Exception(message))

  def failStream[A](message: String): Stream[IO, A] =
    fail[Stream[IO, *], A](message)

  def failPull[A, B](message: String): Pull[IO, A, B] =
    fail[Pull[IO, A, *], B](message)

  def attemptPull[A, B, C]
  (message: String)
  (attempt: Attempt[A])
  (success: A => Pull[IO, B, C])
  : Pull[IO, B, C] =
    attempt
      .fold(
        e => failPull(s"$message: $e"),
        success,
      )

  def attemptF[F[_], A]
  (message: String)
  (attempt: Attempt[A])
  (implicit error: ApplicativeError[F, Throwable])
  : F[A] =
    attempt
      .fold(e => fail[F, A](s"$message: $e"), error.pure)

  def attemptStream[A]
  (message: String)
  (attempt: Attempt[A])
  : Stream[IO, A] =
    attemptF[Stream[IO, *], A](message)(attempt)

  def bytesPull(b: ByteVector): Pull[IO, Byte, Unit] =
    Pull.output(Chunk.byteVector(b))

  def bytes(b: ByteVector): Stream[IO, Byte] =
    Stream.chunk(Chunk.byteVector(b))

  def bits(b: BitVector): Stream[IO, Byte] =
    bytes(b.bytes)

  def bytesPipe: Pipe[IO, ByteVector, Byte] =
    _.flatMap(bytes)

  def bitsPipe: Pipe[IO, BitVector, Byte] =
    _.flatMap(bits)

  def string(s: String): Stream[IO, Byte] =
    Stream.chunk(Chunk.bytes(s.getBytes))

  def pullState[S, A, O]
  (f: S => A => Pull[IO, O, S])
  (in: Stream[IO, A])
  (state: S)
  : Pull[IO, O, S] =
    in.pull.uncons1
      .flatMap {
        case Some((element, tail)) =>
          f(state)(element) >>= pullState(f)(tail)
        case None =>
          Pull.pure(state)
      }
}
