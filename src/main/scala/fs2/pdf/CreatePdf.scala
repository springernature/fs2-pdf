package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._

case class LinearizedPdf(objs: NonEmptyList[IndirectObj], trailer: Trailer, root: Prim.Ref)

object LinearizedPdf
{
  case class State()

  case class FirstPage(root: Pages, objs: List[IndirectObj])

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

  def collectFirstPageObjects(byNumber: Map[Long, IndirectObj], root: Prim): Either[String, List[IndirectObj]] = {
    def spin: Prim => Either[String, List[IndirectObj]] = {
      case ref @ Prim.Ref(number, _) =>
        Either.fromOption(byNumber.lift(number), s"invalid ref to $ref").map(List(_))
      case Prim.Dict(data) =>
        data.values.toList.flatTraverse(spin)
      case Prim.Array(elems) =>
        elems.flatTraverse(spin)
      case _ =>
        Right(Nil)
    }
    spin(root)
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

  def renumberObjs(fp: List[IndirectObj], rest: List[IndirectObj])
  : Either[String, (List[IndirectObj], List[IndirectObj])] = {
    val numbers =
      (rest ++ fp)
        .zipWithIndex
        .map {
          case (IndirectObj(Obj(Obj.Index(number, _), _), _), index) =>
            (number, index)
        }
        .toMap
    (fp.traverse(renumberObj(numbers)), rest.traverse(renumberObj(numbers)))
      .mapN((_, _))
  }

  def firstPage(objs: List[IndirectObj]): Either[String, (FirstPage, List[IndirectObj])] = {
    val byNumber = objsByNumber(objs)
    for {
      root <- Either.fromOption(objs.collectFirst(rootDir), "no root dir")
      first <- resolveFirst(byNumber, root)
      rawFpObjs <- collectFirstPageObjects(byNumber, first.data)
      (fpObjs, rest) <- renumberObjs(rawFpObjs, (Set.from(objs) -- Set.from(rawFpObjs)).toList)
    } yield (FirstPage(root, fpObjs), rest)
  }

  def apply(objs: List[IndirectObj]): Either[String, LinearizedPdf] =
    for {
      (fp, rest) <- firstPage(objs)
    } yield ???
}
