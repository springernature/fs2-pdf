package fs2
package pdf

import cats.data.EitherT
import cats.effect.IO
import fs2.{Pipe, Stream}

object ProcessJarPdf
{
  def processWith[A]
  (doc: String)
  (f: Log => Pipe[IO, Byte, A])
  : EitherT[IO, JarError, Stream[IO, A]] =
    for {
      log <- Log.ioE
      (bytes, _) <- Jar.resourceStream(s"$doc.pdf")
    } yield f(log)(bytes)

  def processWithIO[A]
  (doc: String)
  (f: Log => Stream[IO, Byte] => IO[A])
  : EitherT[IO, JarError, A] =
    for {
      log <- Log.ioE
      (bytes, _) <- Jar.resourceStream(s"$doc.pdf")
      result <- EitherT.liftF(f(log)(bytes))
    } yield result
}
