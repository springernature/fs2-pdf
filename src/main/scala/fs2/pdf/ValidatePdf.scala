package fs2
package pdf

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.IO
import cats.implicits._
import scodec.Attempt

case class ContentRef(owner: Long, target: Long)

case class Refs(contents: List[ContentRef])
{
  def content(add: List[ContentRef]): Refs =
    copy(contents = contents ++ add)
}

sealed trait PdfError

object PdfError
{
  case class Assembly(error: AssemblyError)
  extends PdfError

  case class ContentMissing(ref: ContentRef)
  extends PdfError

  case class ContentStreamMissing(ref: ContentRef)
  extends PdfError

  case class PageMissing(num: Long)
  extends PdfError

  case class InvalidPageTreeObject(obj: IndirectObj)
  extends PdfError

  case object NoPages
  extends PdfError

  case object NoRoot
  extends PdfError

  case object NoCatalog
  extends PdfError

  case object NoPagesInCatalog
  extends PdfError

  case object NoPageRoot
  extends PdfError

  def format: PdfError => String = {
    case Assembly(error) =>
      AssemblyError.format(error)
    case ContentMissing(ContentRef(owner, target)) =>
      s"content object $target missing (owner $owner)"
    case ContentStreamMissing(ContentRef(owner, target)) =>
      s"content object $target has no stream (owner $owner)"
    case PageMissing(num) =>
    s"page $num doesn't exist"
    case InvalidPageTreeObject(obj) =>
      s"object $obj is neither Pages nor Page"
    case NoPages =>
      "no Pages objects in the catalog"
    case NoRoot =>
      "no Root in trailer"
    case NoCatalog =>
      "couldn't find catalog"
    case NoPagesInCatalog =>
      "no Pages in catalog"
    case NoPageRoot =>
    "couldn't find root Pages"
  }
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
      case _ =>
        z
    }

  def indirect(pdf: Pdf): List[IndirectObj] =
    pdf.objs.collect {
      case PdfObj.Content(obj) => obj
      case PdfObj.Data(obj) => IndirectObj(Obj(obj.index, obj.data), None)
    }

  def validateContentStream(byNumber: Map[Long, IndirectObj]): ContentRef => ValidatedNel[PdfError, Unit] = {
    case ref @ ContentRef(owner, target) =>
      byNumber.lift(target) match {
        case None =>
          Validated.Invalid(NonEmptyList.one(PdfError.ContentMissing(ref)))
        case Some(IndirectObj(Obj(_, Prim.refs(refs)), None)) =>
          refs.traverse_(r => validateContentStream(byNumber)(ContentRef(owner, r.number)))
        case Some(IndirectObj(_, None)) =>
          Validated.Invalid(NonEmptyList.one(PdfError.ContentStreamMissing(ref)))
        case _ =>
          Validated.Valid(())
      }
  }

  def collectPages(byNumber: Map[Long, IndirectObj])(root: IndirectObj)
  : ValidatedNel[PdfError, (List[Page], NonEmptyList[Pages])] = {
    def spin(obj: IndirectObj): ValidatedNel[PdfError, (List[Page], List[Pages])] =
      obj match {
        case Pages.obj(pages @ Pages(_, _, kids, _)) =>
          kids
            .map(_.number)
            .foldMap(n => opt(PdfError.PageMissing(n))(byNumber.lift(n)).toList)
            .foldMap(spin)
            .map { case (p, ps) => (p, pages :: ps) }
        case Page.obj(page) =>
          Validated.valid((List(page), Nil))
        case _ =>
          Validated.invalidNel(PdfError.InvalidPageTreeObject(obj))
      }
    spin(root).andThen {
      case (_, Nil) => Validated.invalidNel(PdfError.NoPages)
      case (page1s, pages1 :: pagestail) =>
        Validated.valid((page1s, NonEmptyList(pages1, pagestail)))
    }
  }

  def opt[A](error: PdfError)(oa: Option[A]): ValidatedNel[PdfError, A] =
    Validated.fromOption(oa, NonEmptyList.one(error))

  def att[A](error: PdfError)(aa: Attempt[A]): ValidatedNel[PdfError, A] =
    opt(error)(aa.toOption)

  def validatePages(byNumber: Map[Long, IndirectObj], pdf: Pdf): ValidatedNel[PdfError, Unit] = {
    Validated.fromOption(pdf.trailer.root, NonEmptyList.one(PdfError.NoRoot))
      .andThen(catRef => opt(PdfError.NoCatalog)(byNumber.lift(catRef.number)))
      .andThen(cat => att(PdfError.NoPagesInCatalog)(Prim.Dict.path("Pages")(cat.obj.data)( { case r @ Prim.Ref(_, _) => r } )))
      .andThen(rootRef => opt(PdfError.NoPageRoot)(byNumber.lift(rootRef.number)))
      .andThen(collectPages(byNumber))
      .andThen { a =>
        println(a)
        Validated.valid(())
      }
  }

  def validateContentStreams(byNumber: Map[Long, IndirectObj], refs: Refs): ValidatedNel[PdfError, Unit] =
    refs.contents.foldMap(validateContentStream(byNumber))

  def objsByNumber(pdf: Pdf): Map[Long, IndirectObj] =
    indirect(pdf).map(a => (a.obj.index.number, a)).toMap

  def apply(pdf: Pdf): ValidatedNel[PdfError, Unit] = {
    val byNumber = objsByNumber(pdf)
    val refs = pdf.objs.foldLeft(Refs(Nil))(collectRefs)
    validateContentStreams(byNumber, refs)
      .combine(validatePages(byNumber, pdf))
  }

  def fromDecoded(decoded: Stream[IO, Decoded]): IO[ValidatedNel[PdfError, Unit]] =
    AssemblePdf(decoded)
      .map(
        _
          .leftMap(_.map(PdfError.Assembly(_)))
          .andThen { case ValidatedPdf(pdf, errors) => errors.leftMap(_.map(PdfError.Assembly(_))).combine(apply(pdf)) }
      )
}

sealed trait CompareError

object CompareError
{
  case class DeletedStream(num: Long, data: Prim)
  extends CompareError

  case class ObjectMissing(num: Long, obj: IndirectObj)
  extends CompareError

  case class ObjectAdded(num: Long, obj: IndirectObj)
  extends CompareError

  case class Assembly(error: AssemblyError)
  extends CompareError

  case class Validation(error: PdfError)
  extends CompareError

  def format: CompareError => String = {
    case DeletedStream(num, data) =>
      s"stream deleted from object $num:\n$data"
    case ObjectMissing(num, obj) =>
      s"object $num missing from updated pdf:\n$obj"
    case ObjectAdded(num, obj) =>
      s"object $num added to updated pdf:\n$obj"
    case Assembly(error) =>
      AssemblyError.format(error)
    case Validation(error) =>
      PdfError.format(error)
  }
}

object ComparePdfs
{
  def compareObjs: ((Long, (Option[IndirectObj], Option[IndirectObj]))) => ValidatedNel[CompareError, Unit] = {
    case (num, (Some(IndirectObj(Obj(_, data), Some(_))), Some(IndirectObj(_, None)))) =>
      Validated.invalidNel(CompareError.DeletedStream(num, data))
    case (num, (Some(obj), None)) =>
      Validated.invalidNel(CompareError.ObjectMissing(num, obj))
    case (num, (None, Some(obj))) =>
      Validated.invalidNel(CompareError.ObjectAdded(num, obj))
    case (_, (Some(_), Some(_))) =>
      Validated.Valid(())
  }

  def fromDecoded(oldDecoded: Stream[IO, Decoded], updatedDecoded: Stream[IO, Decoded])
  : IO[ValidatedNel[CompareError, Unit]] =
    (AssemblePdf(oldDecoded), AssemblePdf(updatedDecoded))
      .mapN {
        case (Validated.Valid(old), Validated.Valid(updated)) =>
          val oldByNumber = ValidatePdf.objsByNumber(old.pdf)
          val updatedByNumber = ValidatePdf.objsByNumber(updated.pdf)
          val compared = oldByNumber
            .padZip(updatedByNumber)
            .toList
            .foldMap(compareObjs)
          updated.errors
            .leftMap(_.map(CompareError.Assembly(_)))
            .combine(ValidatePdf(updated.pdf).leftMap(_.map(CompareError.Validation(_))))
            .combine(compared)
        case (a, b) =>
          a.product(b).void.leftMap(_.map(CompareError.Assembly(_)))
      }

  def fromBytes
  (log: Log)
  (oldBytes: Stream[IO, Byte], updatedBytes: Stream[IO, Byte])
  : IO[ValidatedNel[CompareError, Unit]] =
    fromDecoded(PdfStream.decode(log)(oldBytes), PdfStream.decode(log)(updatedBytes))
}
