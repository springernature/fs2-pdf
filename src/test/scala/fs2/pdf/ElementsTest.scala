package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

object ElementsTest
{
  def collect: RewriteState[Unit] => Element => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Element.Meta(trailer, _) =>
        (Nil, state.copy(trailer = Some(trailer)))
      case Element.Data(Obj(index, data), Element.DataKind.Pages(_, _)) =>
        (List(Part.Obj(IndirectObj(index, data, None))), state)
      case _ =>
        (Nil, state)
    }

  def update: RewriteUpdate[Unit] => Part[Trailer] =
    update => Part.Meta(update.trailer)

}

class ElementsTest
extends Specification
{
  def pipe(log: Log): Pipe[IO, Byte, ByteVector] =
    StreamParser.elements(log)
      .andThen(Rewrite.simple(())(ElementsTest.collect)(ElementsTest.update))

  "parse pdf" >>
  ProcessJarPdf.processWith("test-image")(pipe)
    .semiflatMap(_.compile.toList)
    .value
    .unsafeRunSync
    .must(beRight)
}
