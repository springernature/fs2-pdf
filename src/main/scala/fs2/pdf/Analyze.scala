package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt

object Analyze
{
  def analyze: Parsed => Attempt[List[Analyzed]] = {
    case Parsed.IndirectObj(obj, stream, _) =>
      AnalyzeObject(obj, stream)
    case Parsed.StreamObject(obj) =>
      AnalyzeObject(obj, None)
    case Parsed.Unparsable(index, data) =>
      Attempt.successful(List(Analyzed.KeepUnparsable(index, data)))
    case Parsed.Xref(xref) =>
      Attempt.successful(List(Analyzed.Xref(xref)))
    case Parsed.StartXref(startxref) =>
      Attempt.successful(List(Analyzed.StartXref(startxref)))
    case Parsed.Version(version) =>
      Attempt.successful(List(Analyzed.Version(version)))
  }

  def analyzeOrFail(parsed: Parsed): Stream[IO, List[Analyzed]] =
    StreamUtil.attemptStream(s"failed to analyze object: $parsed")(analyze(parsed))

  def analyzed(log: Log): Pipe[IO, Parsed, Analyzed] =
    _
      .through(FilterDuplicates.pipe(log))
      .flatMap(analyzeOrFail)
      .flatMap(Stream.emits)
}
