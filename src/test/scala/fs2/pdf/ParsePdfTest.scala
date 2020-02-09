package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification

class ParsePdfTest
extends Specification
{
  def collect: RewriteState[Unit] => Analyzed => Pull[IO, Part[Trailer], RewriteState[Unit]] =
    state => _ => Pull.pure(state)

  def update: RewriteUpdate[Unit] => Pull[IO, Part[Trailer], Unit] =
    update => Pull.output1(Part.Meta(update.trailer))

  def pipe(log: Log): Pipe[IO, Byte, Byte] =
    StreamParser.objects(log)
      .andThen(AnalyzeObjects.analyzed)
      .andThen(Rewrite(())(collect)(update))
      .andThen(StreamUtil.bytesPipe)

  "parse pdf" >>
  ProcessJarPdf.processWithIO("books/alm")(log => pipe(log)(_).compile.drain)
    .value
    .unsafeRunSync
    .must(beRight)
}
