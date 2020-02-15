package fs2
package pdf

import cats.Foldable
import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import scodec.{Attempt, Err}

object Scodec
{
  def attemptEither[A, B](eab: Either[A, B]): Attempt[B] =
    Attempt.fromEither(eab.leftMap(a => Err(a.toString)))

  def fail[A](message: String): Attempt[A] =
    Attempt.failure(Err(message))

  def attemptNel[T[_]: Foldable, A](desc: String)(as: T[A]): Attempt[NonEmptyList[A]] =
    Attempt.fromOption(NonEmptyList.fromFoldable(as), Err(s"$desc: empty list"))

  def validateAttempt[A]: Attempt[A] => ValidatedNel[String, A] = {
    case Attempt.Successful(a) => Validated.Valid(a)
    case Attempt.Failure(cause) => Validated.invalidNel(cause.messageWithContext)
  }
}
