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
      .andThen(Write.bytes("test-out.pdf"))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/comm")(pipe))
    .unsafeRunSync
    .must_==(())
}
