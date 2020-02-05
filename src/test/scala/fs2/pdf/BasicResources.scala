package fs2
package pdf

import scala.concurrent.ExecutionContext

import cats.effect.{Blocker, ContextShift, IO, Timer}

object BasicResources
{
  implicit def ec: ExecutionContext =
    ExecutionContext.global

  implicit def cs: ContextShift[IO] =
    IO.contextShift(ec)

  implicit def timer: Timer[IO] =
    IO.timer(ec)

  val blocker = Blocker.liftExecutionContext(BasicResources.ec)
}
