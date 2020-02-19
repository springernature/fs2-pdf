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
    val padding = new String(Array.fill(total - numberString.size)(' '))
    s"$numberString$padding"
  }

  def padTera: Long => String = pad(10)

  def pad5: Long => String = pad(5)

  def encodeLinearizationParams(number: Long): LinearizationParams => String = {
    case LinearizationParams(fileSize, firstNumber, hintOffset, hintLength, firstEnd, count, xref) =>
      s"""${pad5(number)} 0 obj
      |<<
      |/Linearized 1.0
      |/L ${padTera(fileSize)}
      |/H [${padTera(hintOffset)} ${padTera(hintLength)}]
      |/O ${pad5(firstNumber)}
      |/E ${padTera(firstEnd)}
      |/N ${padTera(count)}
      |/T ${padTera(xref)}
      |>>
      |endobj
      |""".stripMargin
  }

  def linearizationParams(
    fileSize: Long,
    firstNumber: Long,
    hintOffset: Long,
    hintLength: Long,
    firstEnd: Long,
    count: Long,
    xref: Long,
  ): Attempt[LinearizationParams] =
    Attempt.successful(LinearizationParams(fileSize, firstNumber, hintOffset, hintLength, firstEnd, count, xref))

  val xrefHeadStatic: String =
    s"""xref
    |10000 10000
    |""".stripMargin

  val xrefStatic: String =
    s"""${xrefHeadStatic}trailer
    |<<
    |/Root 10000 0 R
    |/Prev $tera
    |/Size 10000
    |>>
    |startxref
    |$tera
    |%%EOF
    |""".stripMargin

  def xrefSize(entries: Long): Long =
    xrefStatic.size + entries * 20

  def xrefEntries(entries: NonEmptyList[XrefObjMeta], offset: Long): Attempt[NonEmptyList[ByteVector]] =
    GenerateXref.xrefEntries(entries.map(_.index), entries.map(_.size), offset)
      .traverse(a => Codecs.encodeBytes(a._2))

  def xrefHead(number: Long, entries: Long): String =
    s"""xref
    |${pad5(number)} ${pad5(entries)}
    |""".stripMargin

  def trailer(startxref: Long, catalog: Long, size: Long, prev: Long): String =
    s"""trailer
    |<<
    |/Root ${pad5(catalog)} 0 R
    |/Prev ${padTera(prev)}
    |/Size ${pad5(size)}
    |>>
    |startxref
    |${padTera(startxref)}
    |%%EOF
    |""".stripMargin

  def encodeXref(
    entries: NonEmptyList[XrefObjMeta],
    offset: Long,
    fileSize: Long,
    mainXrefOffset: Long,
    catalog: Long,
    root: Long,
    startxref: Long,
    prev: Long,
    extra: List[ByteVector],
    firstNumber: Long,
  ): Attempt[NonEmptyList[ByteVector]] =
    xrefEntries(entries, offset)
      .map { encodedEntries =>
        NonEmptyList(Scodec.stringBytes(xrefHead(firstNumber, entries.size + extra.size)), extra) :::
        encodedEntries :::
        NonEmptyList.one(Scodec.stringBytes(trailer(startxref, catalog, entries.size, prev)))
      }

  def encodeFirstPageXref(
    entries: NonEmptyList[XrefObjMeta],
    offset: Long,
    fileSize: Long,
    mainXrefOffset: Long,
    catalog: Long,
    root: Long,
    startxref: Long,
    prev: Long,
    linNumber: Long,
    linOffset: Long,
  ): Attempt[NonEmptyList[ByteVector]] =
    (xrefEntries(entries, offset), Codecs.encodeBytes(EncodeMeta.objectXrefEntry(Obj.Index(linNumber, 0), linOffset)))
      .mapN { (encodedEntries, linEntry) =>
        Scodec.stringBytes(xrefHead(linNumber, entries.size + 1)) ::
        linEntry ::
        encodedEntries :::
        NonEmptyList.one(Scodec.stringBytes(trailer(startxref, catalog, linNumber + entries.size + 1, prev)))
      }

  def encodeMainXref(
    entries: NonEmptyList[XrefObjMeta],
    offset: Long,
    fileSize: Long,
    mainXrefOffset: Long,
    catalog: Long,
    root: Long,
    startxref: Long,
  ): Attempt[NonEmptyList[ByteVector]] =
    Codecs.encodeBytes(Xref.Entry.freeHead)
      .flatMap { free =>
      encodeXref(
        entries,
        offset,
        fileSize,
        mainXrefOffset,
        catalog,
        root,
        startxref,
        0,
        List(free),
        0,
      )
    }

  def encodeLinearized(pdf: LinearizedPdf): Attempt[NonEmptyList[ByteVector]] =
    for {
      version <- Codecs.encodeBytes(Version.default)
      fp <- (pdf.firstPage.objs).traverse(EncodedObj.indirect)
      main <- (pdf.main).traverse(EncodedObj.indirect)
      linOffset = version.size
      fpSize = fp.map(_.bytes.size).toList.sum
      mainSize = main.map(_.bytes.size).toList.sum
      fpXrefSize = xrefSize(fp.size + 1)
      mainXrefSize = xrefSize(main.size + 1)
      fpXrefOffset = linOffset + maxLinearizationObject.size
      fpOffset = fpXrefOffset + fpXrefSize
      mainOffset = fpOffset + fpSize
      mainXrefOffset = mainOffset + mainSize
      fileSize = mainXrefOffset + mainXrefSize
      pageCount = pdf.firstPage.objs.size + pdf.main.size
      linNumber = pdf.firstPage.objs.head.obj.index.number - 1
      linObj <- linearizationParams(fileSize, pdf.firstPage.firstNumber, 0, 0, mainOffset, pageCount, mainXrefOffset + xrefHeadStatic.size)
        .map(p => Scodec.stringBytes(encodeLinearizationParams(linNumber)(p)))
      linEntry = XrefObjMeta(Obj.Index(linNumber, 0), linOffset)
      fpXref <- encodeFirstPageXref(fp.map(_.xref), fpOffset, fileSize, mainXrefOffset, pdf.firstPage.catalog, pdf.firstPage.root.index.number, 0, mainXrefOffset, linNumber, linOffset)
      mainXref <- encodeMainXref(main.map(_.xref), mainOffset, fileSize, mainXrefOffset, pdf.firstPage.catalog, pdf.firstPage.root.index.number, fpXrefOffset)
    } yield {
      version :: linObj :: fpXref ::: fp.map(_.bytes) ::: main.map(_.bytes) ::: mainXref
    }

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
