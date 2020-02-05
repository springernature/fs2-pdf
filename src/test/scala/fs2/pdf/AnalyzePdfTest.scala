package fs2
package pdf

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import fs2.Stream
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification

class AnalyzePdfTest
extends Specification
{
  def analyzed(bytes: Stream[IO, Byte]): Stream[IO, Unit] =
    bytes
      .through(Pdf.dictOfObj(59))
      .head
      .void

  def runE: EitherT[IO, JarError, Stream[IO, Unit]] =
    for {
      (bytes, _) <- Jar.resourceStream("test-out.pdf")
    } yield analyzed(bytes)

  def run: IO[MatchResult[Any]] =
    runE
      .getOrElse(Stream.empty)
      .flatMap(_.compile.drain)
      .as(1 must_== 1)

  // "analyze pdf" >>
  // run.unsafeRunSync
}
