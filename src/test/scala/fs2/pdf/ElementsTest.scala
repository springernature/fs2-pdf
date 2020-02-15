package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification

object ElementsTest
{
  def collect: RewriteState[Unit] => Element => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Element.Meta(trailer, _) =>
        (Nil, state.copy(trailer = Some(trailer)))
      case Element.Data(Obj(index, data), _) =>
        (List(Part.Obj(IndirectObj(index, data, None))), state)
      case Element.Content(Obj(index, data), rawStream, _, _) =>
        (List(Part.Obj(IndirectObj(index, data, Some(rawStream)))), state)
      case _ =>
        (Nil, state)
    }

  def update: RewriteUpdate[Unit] => Part[Trailer] =
    update => Part.Meta(update.trailer)

}

class ElementsTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, Unit] =
    PdfStream.elements(log)
      .andThen(Rewrite.simple(())(ElementsTest.collect)(ElementsTest.update))
      .andThen(Write.bytes("test-out.pdf"))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/comm")(pipe))
    .unsafeRunSync
    .must_==(())
}
