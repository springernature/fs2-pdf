package fs2
package pdf

import cats.data.EitherT
import cats.effect.IO
import cats.effect.concurrent.Ref
import fs2.Stream
import io.chrisdavenport.log4cats.Logger
import io.chrisdavenport.log4cats.noop.NoOpLogger
import io.chrisdavenport.log4cats.slf4j.Slf4jLogger

case class Log(logger: Logger[IO])
{
  def error(message: => String): IO[Unit] = logger.error(message)
  def debug(message: => String): IO[Unit] = logger.debug(message)

  def errorE[A](message: => String): EitherT[IO, A, Unit] = EitherT.liftF(error(message))
  def debugE[A](message: => String): EitherT[IO, A, Unit] = EitherT.liftF(debug(message))

  def errorES[A](message: => String): EitherT[Stream[IO, ?], A, Unit] = EitherT.liftF(Stream.eval(error(message)))
  def debugES[A](message: => String): EitherT[Stream[IO, ?], A, Unit] = EitherT.liftF(Stream.eval(debug(message)))
}

object Log
{
  def io: IO[Log] =
    Slf4jLogger.create[IO].map(Log(_))

  def ioE[A]: EitherT[IO, A, Log] =
    EitherT.liftF(io)

  def stream: Stream[IO, Log] =
    Stream.eval(io)

  def strict: IO[Log] =
    Ref.of[IO, List[(String, String)]](Nil).map(InMemoryLogger(_)).map(Log(_))

  def noop: Log =
    Log(NoOpLogger.impl)

  def single[A](message: A): IO[Unit] =
    io.flatMap(_.debug(message.toString))

  def singleE[A, B](message: A): EitherT[IO, B, Unit] =
    EitherT.liftF(single(message.toString))
}

case class InMemoryLogger(logs: Ref[IO, List[(String, String)]])
extends Logger[IO]
{
  def log(prio: String, message: String): IO[Unit] =
    logs.update((prio, message) :: _)

  def get: IO[List[(String, String)]] =
    logs.get.map(_.reverse)

  def error(message: => String): IO[Unit] = log("error", message)
  def warn(message: => String): IO[Unit] = log("warn", message)
  def info(message: => String): IO[Unit] = log("info", message)
  def debug(message: => String): IO[Unit] = log("debug", message)
  def trace(message: => String): IO[Unit] = log("trace", message)

  def error(t: Throwable)(message: => String): IO[Unit] = error(message)
  def warn(t: Throwable)(message: => String): IO[Unit] = warn(message)
  def info(t: Throwable)(message: => String): IO[Unit] = info(message)
  def debug(t: Throwable)(message: => String): IO[Unit] = debug(message)
  def trace(t: Throwable)(message: => String): IO[Unit] = trace(message)
}
