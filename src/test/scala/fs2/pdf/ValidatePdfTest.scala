package fs2
package pdf

import cats.data.Validated
import org.specs2.matcher.{Expectable, MatchFailure, MatchResult, MatchSuccess, Matcher}
import org.specs2.mutable.Specification

class ValidatePdfTest
extends Specification
{
  def beValid[A, B]: Matcher[Validated[A, B]] =
    new Matcher[Validated[A, B]] {
      def apply[S <: Validated[A, B]](t: Expectable[S]): MatchResult[S] =
        t.value match {
          case Validated.Valid(_) => MatchSuccess(s"${t.value} is valid", s"${t.value} is valid", t)
          case Validated.Invalid(_) => MatchFailure(s"${t.value} is invalid", s"${t.value} is invalid", t)
        }
    }

  // "validate" >>
  // ProcessJarPdf.processWithIO("books/test-out")(StreamParser.validate).value.map(a => a.must(beRight(beValid))).unsafeRunSync
}
