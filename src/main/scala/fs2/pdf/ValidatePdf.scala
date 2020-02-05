package fs2
package pdf

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.IO
import cats.implicits._
import fs2.Stream

case class ContentRef(owner: Long, target: Long)

case class Refs(contents: List[ContentRef])
{
  def content(add: List[ContentRef]): Refs =
    copy(contents = contents ++ add)
}

object ValidatePdf
{
  def collectRefFromDict(owner: Long, dict: Prim.Dict): List[ContentRef] =
    Prim.Dict.collectRefs("Contents")(dict)
      .toOption
      .map(_.map(ContentRef(owner, _)))
      .combineAll

  def collectRefs(z: Refs, obj: PdfObj): Refs =
    obj match {
      case PdfObj.Indirect(IndirectObj(Obj.Index(number, _), dict @ Prim.Dict(_), _)) =>
        z.content(collectRefFromDict(number, dict))
      case PdfObj.Stream(Obj(Obj.Index(number, _), dict @ Prim.Dict(_))) =>
        z.content(collectRefFromDict(number, dict))
      case PdfObj.Unparsable(_, _) =>
        ???
      case _ =>
        z
    }

  def indirect(pdf: Pdf): List[IndirectObj] =
    pdf.objs.collect { case PdfObj.Indirect(obj) => obj }

  def validateContentStream(byNumber: Map[Long, IndirectObj]): ContentRef => ValidatedNel[String, Unit] = {
    case ContentRef(owner, target) =>
      byNumber.lift(target) match {
        case None =>
          Validated.Invalid(NonEmptyList.one(s"content object $target missing (owner $owner)"))
        case Some(IndirectObj(_, Prim.refs(refs), None)) =>
          refs.traverse_(r => validateContentStream(byNumber)(ContentRef(owner, r.number)))
        case Some(IndirectObj(_, _, None)) =>
          Validated.Invalid(NonEmptyList.one(s"content object $target has no stream (owner $owner)"))
        case _ =>
          Validated.Valid(())
      }
  }

  def validateContentStreams(byNumber: Map[Long, IndirectObj], refs: Refs): ValidatedNel[String, Unit] =
    refs.contents.foldMap(validateContentStream(byNumber))

  def apply(pdf: Pdf): ValidatedNel[String, Unit] = {
    val byNumber = indirect(pdf).map(a => (a.index.number, a)).toMap
    val refs = pdf.objs.foldLeft(Refs(Nil))(collectRefs)
    validateContentStreams(byNumber, refs)
  }

  def fromParsed(parsed: Stream[IO, Parsed]): IO[ValidatedNel[String, Unit]] =
    Pdf.Assemble(parsed)
      .map(_.andThen { case ValidatedPdf(pdf, errors) => errors.combine(apply(pdf)) })

  def diff(left: Pdf, right: Pdf): ValidatedNel[String, Unit] =
    ???
}
