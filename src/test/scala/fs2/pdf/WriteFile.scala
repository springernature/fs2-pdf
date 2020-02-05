package fs2
package pdf

import java.nio.file.{FileSystems, StandardOpenOption}

import cats.effect.IO
import fs2.{Pipe, Stream}

object WriteFile
{
  import BasicResources.cs

  def openOptions: List[StandardOpenOption] =
    List(StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)

  def apply(outfile: String): Pipe[IO, Byte, Unit] =
    bytes =>
      for {
        blocker <- Concurrency.blockerStream
        _ <- bytes.through(fs2.io.file.writeAll[IO](FileSystems.getDefault().getPath(outfile), blocker, openOptions))
      } yield ()

  def io(outfile: String)(bytes: Stream[IO, Byte]): IO[Unit] =
    apply(outfile)(bytes).compile.drain
}
