package fs2
package pdf

import cats.data.ValidatedNel
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.bits.BitVector

object PdfStream
{
  /**
    * Rechunking is crucial for performance, since constructors like [[fs2.io.file.readAll]] use chunk sizes of a few
    * kB, which causes a [[scodec.stream.StreamDecoder]] to parse large objects, like images, for multiple times until
    * they have been read completely.
    *
    * @return Pipe that accumulates 10MB chunks in a byte stream and converts to [[BitVector]].
    */
  def bits: Pipe[IO, Byte, BitVector] =
    _
      .chunkN(10000000)
      .map(_.toBitVector)

  def topLevel: Pipe[IO, Byte, TopLevel] =
    bits
      .andThen(TopLevel.pipe)

  def decode(log: Log): Pipe[IO, Byte, Decoded] =
    bits
      .andThen(Decode.decoded(log))

  def elements(log: Log): Pipe[IO, Byte, Element] =
    decode(log)
      .andThen(Elements.pipe)

  def validate(log: Log)(bytes: Stream[IO, Byte]): IO[ValidatedNel[String, Unit]] =
    ValidatePdf.fromDecoded(decode(log)(bytes))

  def compare(log: Log)(old: Stream[IO, Byte], updated: Stream[IO, Byte]): IO[ValidatedNel[String, Unit]] =
    ComparePdfs.fromDecoded(decode(log)(old), decode(log)(updated))
}