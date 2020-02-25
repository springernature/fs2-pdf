package fs2
package pdf

import org.specs2.mutable.Specification

class StreamDecoderTest
extends Specification
{
  val pdf: String =
    """
    1 1 obj
    <<
    /Type /XRef
    >>
    stream
    00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000
    endstream
    endobj
    """

  "parse pdf" >>
  Stream.emits(pdf.getBytes)
    .through(PdfStream.topLevel)
    .compile
    .to(List)
    .unsafeRunSync
    .must_==(List())
}
