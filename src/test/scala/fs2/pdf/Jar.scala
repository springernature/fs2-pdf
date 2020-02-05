package fs2
package pdf

import java.nio.file.{FileSystems, Files, Path}

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import fs2.Stream

case class JarError(error: String)

object Jar
{
  def io[A](thunk: => A): EitherT[IO, JarError, A] =
    EitherT(IO(thunk).attempt).leftMap(a => JarError(a.getMessage))

  def noResource(name: String): JarError =
    JarError(s"no jar resource found for `$name`")

  def resourcePath(name: String): EitherT[IO, JarError, Path] =
    for {
      res <- EitherT.fromOption[IO](Option(getClass.getClassLoader.getResource(name)), noResource(name))
      file <- EitherT.liftF(IO(res.getFile))
      path <- io(FileSystems.getDefault.getPath(file))
    } yield path

  def resourceBytes(name: String): EitherT[IO, JarError, Array[Byte]] =
    for {
      path <- resourcePath(name)
      bytes <- io(Files.readAllBytes(path))
    } yield bytes

  def resource(name: String): EitherT[IO, JarError, String] =
    resourceBytes(name).map(new String(_))

  def streamFile(path: Path): Stream[IO, Byte] =
    Concurrency.fixedPoolCsStream.flatMap(implicit cs =>
      Concurrency.blockerStream.flatMap(blocker => fs2.io.file.readAll[IO](path, blocker, 8 * 1024))
    )

  def resourceStream(name: String): EitherT[IO, JarError, (Stream[IO, Byte], Long)] =
    for {
      path <- resourcePath(name)
      size <- io(Files.size(path))
    } yield (streamFile(path), size)

  def resourceStreamData(name: String): Stream[IO, Byte] =
    Stream.eval(resourceStream(name).value)
      .flatMap(a => Stream.fromEither[IO](a.leftMap(e => new Exception(e.toString))))
      .flatMap(_._1)
}
