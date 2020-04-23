package fs2
package pdf

import cats.data.Validated
import org.specs2.matcher.{Expectable, MatchFailure, MatchResult, MatchSuccess, Matcher}
import org.specs2.mutable.Specification

object ValidatedMatcher
{
  def beValid[A, B]: Matcher[Validated[A, B]] =
    new Matcher[Validated[A, B]] {
      def apply[S <: Validated[A, B]](t: Expectable[S]): MatchResult[S] =
        t.value match {
          case Validated.Valid(_) => MatchSuccess(s"${t.value} is valid", s"${t.value} is valid", t)
          case Validated.Invalid(_) => MatchFailure(s"${t.value} is invalid", s"${t.value} is invalid", t)
        }
    }
}

class ValidatePdfTest
extends Specification
{
  import ValidatedMatcher.beValid

  // "validate" >>
  // ProcessJarPdf.processWithIO("books/broken-comm")(PdfStream.validate).value.map(a => a.must(beRight(beValid))).unsafeRunSync

  "validate empty kids" >>
    ProcessJarPdf.processWithIO("empty-kids")(PdfStream.validate)
      .value.map(a => a.must(beRight(beValid))).unsafeRunSync
}
