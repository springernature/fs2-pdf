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

  "parse pdf with empty kids" >>
    ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("empty-kids")(pipe))
      .unsafeRunSync
      .must_==(())
}
