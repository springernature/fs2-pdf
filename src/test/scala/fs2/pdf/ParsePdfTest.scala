package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

object ParseTest
{
  def collect: RewriteState[Unit] => Element => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Element.Data(obj, Element.DataKind.Pages(_)) =>
        (List(Part.Obj(IndirectObj(obj.index, obj.data, None))), state)
    case Element.Meta(trailer, _) =>
        (Nil, state.copy(trailer = Some(trailer)))
      case _ =>
        (Nil, state)
    }

  def update: RewriteUpdate[Unit] => Part[Trailer] =
    update => Part.Meta(update.trailer)

}

class ParsePdfTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, ByteVector] =
    StreamParser.elements(log)
      .andThen(Rewrite.simple(())(ParseTest.collect)(ParseTest.update))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/biomech")(pipe))
    .unsafeRunSync
    .must_==(())
}
