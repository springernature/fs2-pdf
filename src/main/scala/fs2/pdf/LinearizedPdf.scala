package fs2
package pdf

import cats.Eval
import cats.data.{EitherT, NonEmptyList}
import cats.implicits._

case class LinearizedPdf(
  firstPage: LinearizedPdf.FirstPage,
  main: NonEmptyList[IndirectObj],
)

object LinearizedPdf
{
  case class State()

  case class FirstPage(root: Pages, firstNumber: Long, catalog: Long, objs: NonEmptyList[IndirectObj])

  def objsByNumber(objs: List[IndirectObj]): Map[Long, IndirectObj] =
    objs.map(a => (a.obj.index.number, a)).toMap

  def rootDir: PartialFunction[IndirectObj, Pages] = {
    case Pages.obj(pages @ Pages(_, _, _, true)) => pages
  }

  def resolveFirst(byNumber: Map[Long, IndirectObj], root: Pages): Either[String, Page] = {
    Either.fromOption(byNumber.lift(root.kids.head.number), s"missing object for leftmost page in $root")
      .flatMap {
        case Page.obj(page) =>
          Right(page)
        case Pages.obj(pages) =>
          resolveFirst(byNumber, pages)
        case obj =>
          Left(s"invalid object in page tree: $obj")
      }
  }

  def collectFirstPageObjects(byNumber: Map[Long, IndirectObj], first: Page)
  : Either[String, (NonEmptyList[IndirectObj])] = {
    def spin(seen: Set[Long]): Prim => EitherT[Eval, String, Set[Long]] = {
      case ref @ Prim.Ref(number, _) if !seen.contains(number) =>
        EitherT.fromOption[Eval](byNumber.lift(number), s"invalid ref to $ref")
          .flatMap { referenced =>
            spin(seen + number)(referenced.obj.data)
          }
      case Prim.Dict(data) =>
        data.filterNot(_._1 == "Parent").values.toList.foldM(seen)((z, a) => spin(z)(a))
      case Prim.Array(elems) =>
        elems.foldM(seen)((z, a) => spin(z)(a))
      case _ =>
        EitherT.pure(seen)
    }
    spin(Set(first.index.number))(first.data)
      .value
      .value
      .flatMap(_.toList.traverse(num => Either.fromOption(byNumber.lift(num), s"invalid ref to $num")))
      .map(NonEmptyList.fromList(_))
      .flatMap(Either.fromOption(_, "no objects for first page"))
  }

  def renumberPrim(numbers: Map[Long, Int]): Prim => Either[String, Prim] = {
    case Prim.Dict(data) =>
      data.toList.traverse { case (k, v) => renumberPrim(numbers)(v).map((k, _)) }.map(_.toMap).map(Prim.Dict(_))
    case Prim.Array(data) =>
      data.traverse(renumberPrim(numbers)).map(Prim.Array(_))
    case Prim.Ref(oldNumber, _) =>
      Either.fromOption(numbers.lift(oldNumber), s"old number not in number map: $oldNumber")
        .map(Prim.Ref(_, 0))
    case prim =>
      Right(prim)
  }

  def renumberObj(numbers: Map[Long, Int]): IndirectObj => Either[String, IndirectObj] = {
    case IndirectObj(Obj(Obj.Index(oldNumber, _), data), stream) =>
      for {
        newNumber <- Either.fromOption(numbers.lift(oldNumber), s"old number not in number map: $oldNumber")
        newData <- renumberPrim(numbers)(data)
      } yield IndirectObj(Obj(Obj.Index(newNumber, 0), newData), stream)
  }

  def findCatalog(objs: List[IndirectObj]): Either[String, IndirectObj] =
    Either.fromOption(
      objs.collectFirst { case obj @ IndirectObj(Obj.tpe("Catalog", _), None) => obj },
      "no catalog in PDF",
    )

  def renumberObjs(firstPage: Long, catalog: Long, fp: NonEmptyList[IndirectObj], main: NonEmptyList[IndirectObj])
  : Either[String, (Long, Long, NonEmptyList[IndirectObj], NonEmptyList[IndirectObj])] = {
    val numbers =
      (main.toList ++ (IndirectObj.nostream(-1, Prim.Null) :: fp.toList))
        .zipWithIndex
        .map {
          case (IndirectObj(Obj(Obj.Index(number, _), _), _), index) =>
            (number, index + 1)
        }
        .toMap
    (
      Either.fromOption(numbers.lift(firstPage), "first page number not in numbers dict"),
      Either.fromOption(numbers.lift(catalog), "catalog number not in numbers dict"),
      fp.traverse(renumberObj(numbers)),
      main.traverse(renumberObj(numbers)),
    )
      .mapN((_, _, _, _))
  }

  def mainObjs(objs: List[IndirectObj], fpObjs: List[IndirectObj]): Either[String, NonEmptyList[IndirectObj]] =
    Either.fromOption(
      NonEmptyList.fromList((Set.from(objs) -- Set.from(fpObjs)).toList),
      "no objects for main part of document",
    )

  def firstPage(objs: List[IndirectObj]): Either[String, (FirstPage, NonEmptyList[IndirectObj])] = {
    val byNumber = objsByNumber(objs)
    for {
      root <- Either.fromOption(objs.collectFirst(rootDir), "no root dir")
      first <- resolveFirst(byNumber, root)
      fpRefObjs <- collectFirstPageObjects(byNumber, first)
      catalog <- findCatalog(objs)
      fpObjs = Pages.indirectObj(root) :: catalog :: fpRefObjs
      rawMain <- mainObjs(objs, fpObjs.toList)
      (fpNumber, catalogNumber, fpObjs, main) <- renumberObjs(first.index.number, catalog.obj.index.number, fpObjs, rawMain)
    } yield {
      println(fpObjs)
      (FirstPage(root, fpNumber, catalogNumber, fpObjs), main)
    }
  }

  def apply(objs: List[IndirectObj]): Either[String, LinearizedPdf] =
    for {
      (fp, main) <- firstPage(objs)
    } yield LinearizedPdf(fp, main)
}
