package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt

object Analyze
{
  def analyze: Parsed => Attempt[List[Analyzed]] = {
    case Parsed.Raw(data) =>
      AnalyzeNonObject(data)
    case Parsed.IndirectObj(obj, stream, _) =>
      AnalyzeObject(obj, stream)
    case Parsed.StreamObject(obj) =>
      AnalyzeObject(obj, None)
    case Parsed.Unparsable(index, data) =>
      Attempt.successful(List(Analyzed.KeepUnparsable(index, data)))
  }

  def analyzeOrFail(parsed: Parsed): Stream[IO, List[Analyzed]] =
    StreamUtil.attemptStream(s"failed to analyze object: $parsed")(analyze(parsed))

  def analyzed(log: Log): Pipe[IO, Parsed, Analyzed] =
    _
      // .through(FilterDuplicates.pipe(log))
      .flatMap(analyzeOrFail)
      .flatMap(Stream.emits)
}
