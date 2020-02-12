package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

object ParseTest
{
  def collect: RewriteState[Unit] => Analyzed => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Analyzed.Xref(Xref(_, trailer, _)) =>
        (Nil, state.copy(trailers = trailer :: state.trailers))
      case Analyzed.XrefStream(XrefStream(_, trailer)) =>
        (Nil, state.copy(trailers = trailer :: state.trailers))
      case Analyzed.PageDir(dir) =>
        (List(Part.Obj(IndirectObj(dir.index, dir.data, None))), state)
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
    StreamParser.objects(log)
      .andThen(Analyze.analyzed(log))
      .andThen(Rewrite.simple(())(ParseTest.collect)(ParseTest.update))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/biomech")(pipe))
    .unsafeRunSync
    .must_==(())
}
