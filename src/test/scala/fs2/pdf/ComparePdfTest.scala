package fs2
package pdf

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import org.specs2.mutable.Specification

class ComparePdfTest
extends Specification
{
  import ValidatedMatcher.beValid

  def report: ValidatedNel[String, Unit] => ValidatedNel[String, Unit] = {
    case Validated.Valid(()) => Validated.Valid(())
    case Validated.Invalid(errors) =>
      errors.toList.foreach(println)
      Validated.invalidNel(s"${errors.size} errors")
  }

//   "validate" >>
//   ProcessJarPdf.processWithIO("books/broken-biomech") { _ => updated =>
//     ProcessJarPdf.processWithIO("books/biomech") { log => old =>
//       PdfStream.compare(log)(old, updated)
//     }.value
//   }
//     .value
//     .unsafeRunSync
//     .flatten
//     .map(report)
//     .as(())
//     .must(beRight(()))
}
