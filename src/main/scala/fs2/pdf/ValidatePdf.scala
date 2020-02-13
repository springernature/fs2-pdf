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

  def objsByNumber(pdf: Pdf): Map[Long, IndirectObj] =
    indirect(pdf).map(a => (a.index.number, a)).toMap

  def apply(pdf: Pdf): ValidatedNel[String, Unit] = {
    val refs = pdf.objs.foldLeft(Refs(Nil))(collectRefs)
    validateContentStreams(objsByNumber(pdf), refs)
  }

  def fromParsed(parsed: Stream[IO, Parsed]): IO[ValidatedNel[String, Unit]] =
    Pdf.Assemble(parsed)
      .map(_.andThen { case ValidatedPdf(pdf, errors) => errors.combine(apply(pdf)) })
}

object ComparePdfs
{
  def compareObjs: ((Long, (Option[IndirectObj], Option[IndirectObj]))) => ValidatedNel[String, Unit] = {
    case (num, (Some(IndirectObj(_, data, Some(_))), Some(IndirectObj(_, _, None)))) =>
      Validated.invalidNel(s"stream deleted from object $num:\n$data")
    case (num, (Some(a), None)) =>
      Validated.invalidNel(s"object $num missing from updated pdf:\n$a")
    case (num, (None, Some(a))) =>
      Validated.invalidNel(s"object $num added to updated pdf:\n$a")
    case (_, (Some(_), Some(_))) =>
      Validated.Valid(())
  }

  def fromParsed(oldParsed: Stream[IO, Parsed], updatedParsed: Stream[IO, Parsed]): IO[ValidatedNel[String, Unit]] =
    (Pdf.Assemble(oldParsed), Pdf.Assemble(updatedParsed))
      .mapN {
        case (Validated.Valid(old), Validated.Valid(updated)) =>
          val oldByNumber = ValidatePdf.objsByNumber(old.pdf)
          val updatedByNumber = ValidatePdf.objsByNumber(updated.pdf)
          oldByNumber
            .padZip(updatedByNumber)
            .toList
            .foldMap(compareObjs)
        case (_, _) =>
          ???
      }
}
