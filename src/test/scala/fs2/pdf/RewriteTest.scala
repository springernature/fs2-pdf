package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification

class RewriteTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, Unit] =
    PdfStream.elements(log)
      .andThen(Element.encode)
      .andThen(Write.bytes("books/test-out.pdf"))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/mod")(pipe))
    .unsafeRunSync
    .must_==(())
}
