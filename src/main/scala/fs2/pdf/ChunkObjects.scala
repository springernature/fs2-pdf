package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.Decoder
import scodec.bits.{BitVector, ByteVector}
import scodec.stream.StreamDecoder

sealed trait RawPdfChunk

object RawPdfChunk
{
  case class Rest(bytes: ByteVector)
  extends RawPdfChunk

  case class Object(bytes: ByteVector)
  extends RawPdfChunk
}

object ChunkObjects
{
  val objectEndMarker: ByteVector =
    ByteVector("endobj".getBytes)

  def findNextObject: Decoder[RawPdfChunk] =
    Decoder(bits => Codecs.takeBytesUntilAfter(objectEndMarker, Codecs.multiWhitespaceDecoder)(bits.bytes))
      .map(RawPdfChunk.Object(_))

  def chunkObjects: StreamDecoder[RawPdfChunk] =
    StreamDecoder.many(findNextObject)

  /**
   * This might be easier to write, but there seems to be no "fold everything" Pull.
   * Since the StreamDecoder in chunkObjects returns the remaining input Stream in a Pull, the signature is
   * predetermined.
   */
  def rest(in: Stream[IO, BitVector]): Pull[IO, RawPdfChunk, Unit] =
    in.pull.fold(BitVector.empty)(_ ++ _)
      .flatMap(bits => Pull.output1(RawPdfChunk.Rest(bits.bytes)))

  val pipe: Pipe[IO, BitVector, RawPdfChunk] =
    in =>
      chunkObjects(in)
        .flatMap(_.traverse_(rest))
        .stream
}
