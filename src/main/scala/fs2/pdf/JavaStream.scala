package fs2
package pdf

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}

import cats.effect.IO
import cats.implicits._

object JavaStream
{
  def baos: ByteArrayOutputStream =
    new ByteArrayOutputStream()

  def withBaos(f: OutputStream => IO[Unit]): IO[Array[Byte]] = {
    val os = baos
    f(os) *> IO(os.toByteArray)
  }

  def bais(bytes: Array[Byte]): ByteArrayInputStream =
    new ByteArrayInputStream(bytes)

  def withBaisStrict[A](bytes: Array[Byte])(f: InputStream => IO[A]): IO[A] = {
    f(bais(bytes))
  }

  def withByteStreams(bytes: Array[Byte])(f: (InputStream, OutputStream) => IO[Unit]): IO[Array[Byte]] =
    withBaisStrict(bytes)(is => withBaos(os => f(is, os)))
}
