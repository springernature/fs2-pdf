package fs2
package pdf

import cats.data.ValidatedNel
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.bits.{BitVector, ByteVector}

/**
  * The main API for this library provides [[Pipe]]s for decoding, encoding and validating PDF streams.
  */
object PdfStream
{
  /**
    * Rechunking is crucial for performance, since constructors like [[fs2.io.file.readAll]] use chunk sizes of a few
    * kB, which causes a [[scodec.stream.StreamDecoder]] to parse large objects, like images, for multiple times until
    * they have been read completely.
    *
    * @return [[Pipe]] that accumulates 10MB chunks in a byte stream and converts to [[BitVector]].
    */
  def bits: Pipe[IO, Byte, BitVector] =
    _
      .chunkN(10000000)
      .map(_.toBitVector)

  /**
    * Decode indirect objects, version header, comments, xrefs and lone startxrefs for xref streams.
    *
    * @return [[Pipe]] that decodes a byte [[Stream]] into basic PDF components encoded as the ADT [[TopLevel]].
    */
  def topLevel: Pipe[IO, Byte, TopLevel] =
    bits
      .andThen(TopLevel.pipe)

  /**
    * Decode data objects (without stream), content objects (with stream), and metadata information consisting of
    * [[Xref]]s, [[Trailer]]s and [[Version]].
    * Content objects contain both raw streams as well as an instance of [[Uncompressed]], which contains a lazily
    * evaluated [[BitVector]] that is either zlib- and/or predictor-inflated or the original.
    *
    * @param log a logger of type [[Log]]
    * @return [[Pipe]] that decodes a byte [[Stream]] into [[Decoded]]
    */
  def decode(log: Log): Pipe[IO, Byte, Decoded] =
    bits
      .andThen(Decode(log))

  /**
    * Decode high-level PDF components like page objects, page tree objects, indirect arrays, resource dictionaries with
    * font references, and images.
    * Metainformation consists of an accumulated [[Trailer]] and the [[Version]].
    *
    * @param log a logger of type [[Log]]
    * @return [[Pipe]] that decodes a byte [[Stream]] into [[Element]]
    */
  def elements(log: Log): Pipe[IO, Byte, Element] =
    decode(log)
      .andThen(Elements.pipe)

  /**
    * After transforming the data obtained from the decoder pipes into [[Part]] records, this will encode PDF objects
    * and generate an appropriate xref.
    *
    * @return [[Pipe]] that encodes a [[Part]] [[Stream]] into [[ByteVector]]s.
    */
  def write: Pipe[IO, Part[Trailer], ByteVector] =
    WritePdf.parts

  /**
    * This validation routine parses and inspects a PDF for errors.
    *
    * All references in /Content arrays are collected and the dereferenced objects are ensured to be content streams.
    * The /Kids references of all page tree objects are ensured to be /Page objects.
    *
    * @param log a logger of type [[Log]]
    * @param bytes PDF data [[Stream]]
    * @return a list of errors in the PDF
    */
  def validate(log: Log)(bytes: Stream[IO, Byte]): IO[ValidatedNel[PdfError, Unit]] =
    ValidatePdf.fromDecoded(decode(log)(bytes))

  /**
    * This comparison routine analyzes the differences between two PDF documents.
    *
    * When transforming a PDF with this library, it is necessary to compare the before and after states, especially in
    * order to find errors in the transformation process.
    * All objects with the same number are inspected as to whether they both contain or don't contain streams.
    *
    * @param log a logger of type [[Log]]
    * @param old the PDF before the transformation
    * @param updated the PDF after the transformation
    * @return a list of differences of the two PDFs
    */
  def compare(log: Log)(old: Stream[IO, Byte], updated: Stream[IO, Byte]): IO[ValidatedNel[CompareError, Unit]] =
    ComparePdfs.fromDecoded(decode(log)(old), decode(log)(updated))
}
