package fs2
package pdf

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.IO
import cats.implicits._
import fs2.Stream
import scodec.Attempt

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
      case PdfObj.Content(IndirectObj.dict(number, dict)) =>
        z.content(collectRefFromDict(number, dict))
      case PdfObj.Data(Obj.dict(number, dict)) =>
        z.content(collectRefFromDict(number, dict))
      case PdfObj.Unparsable(_, _) =>
        ???
      case _ =>
        z
    }

  def indirect(pdf: Pdf): List[IndirectObj] =
    pdf.objs.collect {
      case PdfObj.Content(obj) => obj
      case PdfObj.Data(obj) => IndirectObj(Obj(obj.index, obj.data), None)
    }

  def validateContentStream(byNumber: Map[Long, IndirectObj]): ContentRef => ValidatedNel[String, Unit] = {
    case ContentRef(owner, target) =>
      byNumber.lift(target) match {
        case None =>
          Validated.Invalid(NonEmptyList.one(s"content object $target missing (owner $owner)"))
        case Some(IndirectObj(Obj(_, Prim.refs(refs)), None)) =>
          refs.traverse_(r => validateContentStream(byNumber)(ContentRef(owner, r.number)))
        case Some(IndirectObj(_, None)) =>
          Validated.Invalid(NonEmptyList.one(s"content object $target has no stream (owner $owner)"))
        case _ =>
          Validated.Valid(())
      }
  }

  def collectPages(byNumber: Map[Long, IndirectObj])(root: IndirectObj)
  : ValidatedNel[String, (NonEmptyList[Page], NonEmptyList[Pages])] = {
    def spin(obj: IndirectObj): ValidatedNel[String, (List[Page], List[Pages])] =
      obj match {
        case Pages.obj(pages @ Pages(_, _, kids, _)) =>
          kids
            .map(_.number)
            .foldMap(n => opt(s"page $n doesn't exist")(byNumber.lift(n)).toList)
            .foldMap(spin)
            .map { case (p, ps) => (p, pages :: ps) }
        case Page.obj(page) =>
          Validated.valid((List(page), Nil))
        case _ =>
          Validated.invalidNel(s"object $obj is neither Pages nor Page")
      }
    spin(root).andThen {
      case (_, Nil) => Validated.invalidNel("no Pages objects in the catalog")
      case (Nil, _) => Validated.invalidNel("no Page objects in the page tree")
      case (page1 :: pagetail, pages1 :: pagestail) =>
        Validated.valid((NonEmptyList(page1, pagetail), NonEmptyList(pages1, pagestail)))
    }
  }

  def opt[A](error: String)(oa: Option[A]): ValidatedNel[String, A] =
    Validated.fromOption(oa, NonEmptyList.one(error))

  def att[A](error: String)(aa: Attempt[A]): ValidatedNel[String, A] =
    opt(error)(aa.toOption)

  def validatePages(byNumber: Map[Long, IndirectObj], pdf: Pdf): ValidatedNel[String, Unit] = {
    Validated.fromOption(pdf.trailer.root, NonEmptyList.one("no Root in trailer"))
      .andThen(catRef => opt("couldn't find catalog")(byNumber.lift(catRef.number)))
      .andThen(cat => att("no Pages in catalog")(Prim.Dict.path("Pages")(cat.obj.data)( { case r @ Prim.Ref(_, _) => r } )))
      .andThen(rootRef => opt("couldn't find root Pages")(byNumber.lift(rootRef.number)))
      .andThen(collectPages(byNumber))
      .andThen { a =>
        println(a)
        Validated.valid(())
      }
  }

  def validateContentStreams(byNumber: Map[Long, IndirectObj], refs: Refs): ValidatedNel[String, Unit] =
    refs.contents.foldMap(validateContentStream(byNumber))

  def objsByNumber(pdf: Pdf): Map[Long, IndirectObj] =
    indirect(pdf).map(a => (a.obj.index.number, a)).toMap

  def apply(pdf: Pdf): ValidatedNel[String, Unit] = {
    val byNumber = objsByNumber(pdf)
    val refs = pdf.objs.foldLeft(Refs(Nil))(collectRefs)
    validateContentStreams(byNumber, refs)
      .combine(validatePages(byNumber, pdf))
  }

  def fromDecoded(decoded: Stream[IO, Decoded]): IO[ValidatedNel[String, Unit]] =
    Pdf.Assemble(decoded)
      .map(_.andThen { case ValidatedPdf(pdf, errors) => errors.combine(apply(pdf)) })
}

object ComparePdfs
{
  def compareObjs: ((Long, (Option[IndirectObj], Option[IndirectObj]))) => ValidatedNel[String, Unit] = {
    case (num, (Some(IndirectObj(Obj(_, data), Some(_))), Some(IndirectObj(_, None)))) =>
      Validated.invalidNel(s"stream deleted from object $num:\n$data")
    case (num, (Some(a), None)) =>
      Validated.invalidNel(s"object $num missing from updated pdf:\n$a")
    case (num, (None, Some(a))) =>
      Validated.invalidNel(s"object $num added to updated pdf:\n$a")
    case (_, (Some(_), Some(_))) =>
      Validated.Valid(())
  }

  def fromDecoded(oldDecoded: Stream[IO, Decoded], updatedDecoded: Stream[IO, Decoded]): IO[ValidatedNel[String, Unit]] =
    (Pdf.Assemble(oldDecoded), Pdf.Assemble(updatedDecoded))
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
