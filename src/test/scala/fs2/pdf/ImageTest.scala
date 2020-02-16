package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class ImageTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, ByteVector] =
    PdfStream.elements(log)
      .andThen(Element.encode)

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("test-image")(pipe))
    .unsafeRunSync
    .must_==(())
}
