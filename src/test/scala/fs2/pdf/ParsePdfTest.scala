package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class ParsePdfTest
extends Specification
{
  def collect: RewriteState[Unit] => Analyzed => Pull[IO, Part[Trailer], RewriteState[Unit]] =
    state => {
      case Analyzed.Xref(Xref(_, trailer, _)) =>
        Pull.pure(state.copy(trailers = trailer :: state.trailers))
      case Analyzed.XrefStream(XrefStream(_, trailer)) =>
        Pull.pure(state.copy(trailers = trailer :: state.trailers))
      case _ =>
        Pull.pure(state)
    }

  def update: RewriteUpdate[Unit] => Pull[IO, Part[Trailer], Unit] =
    update => Pull.output1(Part.Meta(update.trailer))

  def pipe(log: Log): Pipe[IO, Byte, ByteVector] =
    StreamParser.objects(log)
      .andThen(Analyze.analyzed)
      .andThen(Rewrite(())(collect)(update))

  "parse pdf" >>
  ProcessJarPdf.ignoreError(ProcessJarPdf.processWith("books/semi")(pipe))
    .unsafeRunSync
    .must_==(())
}
