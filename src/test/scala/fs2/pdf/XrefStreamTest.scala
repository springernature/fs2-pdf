package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class XrefStreamTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, ByteVector] =
    PdfStream.elements(log)
      .andThen(Element.encode)

  "parse an xref stream" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("xref-stream")(pipe))
    .unsafeRunSync
    .must_==(())
}
