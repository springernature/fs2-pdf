package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import codec.Codecs
import fs2.{Pipe, Stream}
import scodec.Attempt
import scodec.bits.ByteVector
import scodec.interop.cats.AttemptMonadErrorInstance

object WriteLinearized
{
  def objectNumber[A]: Part[A] => Attempt[Long] = {
    case Part.Obj(IndirectObj(Obj(Obj.Index(n, _), _), _)) => Attempt.successful(n)
    case _ => Scodec.fail("first part is not an object")
  }

  def encode[A]
  : Part[A] => Attempt[EncodedObj] = {
    case Part.Obj(obj) =>
      EncodedObj.indirect(obj)
    case Part.Meta(_) =>
      Scodec.fail("trailer in first page data")
    case Part.Version(_) =>
      Scodec.fail("Part.Version not at the head of stream")
  }

  case class LinearizationParams(
    fileSize: Long,
    firstPageObjNumber: Long,
    hintStreamOffset: Long,
    hintStreamLength: Long,
    firstPageEndOffset: Long,
    pageCount: Long,
    mainXrefOffset: Long,
  )

  def linearizationDict: LinearizationParams => Prim.Dict = {
    case LinearizationParams(
      fileSize,
      firstPageObjNumber,
      hintStreamOffset,
      hintStreamLength,
      firstPageEndOffset,
      pageCount,
      mainXrefOffset,
    ) =>
      Prim.dict(
        "Linearized" -> Prim.Number(1),
        "L" -> Prim.Number(fileSize),
        "H" -> Prim.Array.nums(hintStreamOffset, hintStreamLength),
        "O" -> Prim.Number(firstPageObjNumber),
        "E" -> Prim.Number(firstPageEndOffset),
        "N" -> Prim.Number(pageCount),
        "T" -> Prim.Number(mainXrefOffset),
      )
  }

  def linParams(totalCount: Int, fileSize: Long): LinearizationParams =
    LinearizationParams(
      fileSize,
      0,
      0,
      0,
      0,
      totalCount,
      0,
    )

  val tera: Long =
    1000000000

  def maxLinearizationObject: String =
    s"""10000 0 obj
    |<<
    |/Linearized 1.0
    |/L $tera
    |/H [$tera $tera]
    |/O 10000
    |/E $tera
    |/N $tera
    |/T $tera
    |>>
    |endobj
    |""".stripMargin

  def pad(total: Int)(number: Long): String = {
    val numberString = number.toString
    s"${new String(Array.fill(total - numberString.size)(' '))}$numberString"
  }

  def padTera: Long => String = pad(10)

  def encodeLinearizationParams(number: Long): LinearizationParams => String = {
    case LinearizationParams(fileSize, firstNumber, hintOffset, hintLength, firstEnd, count, xref) =>
      s"""$number 0 obj
      |<<
      |/Linearized 1.0
      |/L ${padTera(fileSize)}
      |/H [${padTera(hintOffset)} ${padTera(hintLength)}]
      |/O ${pad(5)(firstNumber)}
      |/E ${padTera(firstEnd)}
      |/N ${padTera(count)}
      |/T ${padTera(xref)}
      |>>
      |endobj
      |""".stripMargin
  }

  def linearizationParams(offset: Long, firstPage: NonEmptyList[ByteVector], rest: NonEmptyList[ByteVector])
  : Attempt[LinearizationParams] =
    ???

  def encodeFirstPageXref(entries: NonEmptyList[XrefObjMeta], catalog: Long, root: Long): String =
    s"""xref
    |
    |""".stripMargin

  def encodeMainXref(offset: Long): Attempt[ByteVector] =
    ???

  val xrefStatic: String =
    s"""xref
    |10000 10000
    |trailer
    |<<
    |/Root 10000 0 R
    |/Catalog 10000 0 R
    |/Size 10000
    |>>
    |startxref
    |$tera
    |%%EOF
    |""".stripMargin

  def xrefSize(entries: Long): Long =
    xrefStatic.size + entries * 20

  def encodeLinearized(pdf: LinearizedPdf): Attempt[NonEmptyList[ByteVector]] =
    for {
      version <- Codecs.encodeBytes(Version.default)
      fp <- (pdf.firstPage.objs).traverse(Codecs.encodeBytes[IndirectObj])
      rest <- (pdf.rest).traverse(Codecs.encodeBytes[IndirectObj])
      fpSize = fp.map(_.size).toList.sum
      restSize = rest.map(_.size).toList.sum
      fpXrefSize = xrefSize(fp.size)
      restXrefSize = xrefSize(rest.size + 1)
      restOffset = version.size + maxLinearizationObject.size + fpXrefSize + fpSize
      restXrefOffset = restOffset + restSize
      fileSize = restXrefOffset + restXrefSize
      fpXref <- Scodec.stringBytes(encodeFirstPageXref(fileSize, restXrefOffset))
      mainXref <- encodeMainXref(version.size + fpXref.size)
      linDict <- linearizationParams(version.size, fp, rest)
        .map(p => Scodec.stringBytes(encodeLinearizationParams(pdf.firstPage.objs.head.obj.index.number - 1)(p)))
    } yield version :: linDict :: fpXref :: fp ::: (mainXref :: rest)

  def write(pdf: LinearizedPdf): Stream[IO, ByteVector] =
    StreamUtil.attemptStream("encoding linearized objects")(encodeLinearized(pdf))
      .map(_.toList)
      .flatMap(Stream.emits(_))

  def fresh: Pipe[IO, IndirectObj, ByteVector] =
    in =>
      Stream.eval(in.compile.to(List).map(LinearizedPdf.apply))
        .flatMap {
          case Left(error) => StreamUtil.failStream(error)
          case Right(pdf) => write(pdf)
        }
}
