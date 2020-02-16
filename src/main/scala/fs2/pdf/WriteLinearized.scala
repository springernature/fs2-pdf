package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.ByteVector
import scodec.interop.cats.AttemptMonadErrorInstance

object WriteLinearized
{
  def objectNumber[A]: Part[A] => Attempt[Long] = {
    case Part.Obj(IndirectObj(Obj(Obj.Index(n, _), _), _)) => Attempt.successful(n)
    case Part.Unparsable(Obj.Index(n, _), _) => Attempt.successful(n)
    case _ => Scodec.fail("first part is not an object")
  }

  def encode[A]
  : Part[A] => Attempt[EncodedObj] = {
    case Part.Obj(obj) =>
      EncodedObj.indirect(obj)
    case Part.Unparsable(index, data) =>
      Attempt.successful(EncodedObj(XrefObjMeta(index, data.size), data))
    case Part.Meta(_) =>
      Scodec.fail("trailer in first page data")
    case Part.Version(_) =>
      Scodec.fail("Part.Version not at the head of stream")
  }

  val xrefStatic: String =
    """xref
    |
    |trailer
    |
    |startxref
    |0
    |%%EOF
    |""".stripMargin

  def calculateXrefLength(entries: Int, totalCount: Int, trailer: Prim.Dict): Attempt[Long] =
    Prim.Codec_Prim.encode(trailer)
      .map { encTrailer =>
        encTrailer.bytes.size +
        entries * 20 +
        entries.toString.size +
        (totalCount - entries + 1).toString.size +
        1 +
        xrefStatic.size
      }

  def encodeFirstPageParts[A]
  (firstPage: NonEmptyList[Part[A]], count: Int, trailer: Prim.Dict)
  : Attempt[(NonEmptyList[XrefObjMeta], NonEmptyList[ByteVector], Trailer)] =
    firstPage.traverse(encode)
      .map { encoded =>
        (
          encoded.map(_.xref),
          encoded.map(_.bytes),
          Trailer(count, trailer ++ Prim.dict("Size" -> Prim.num(count)), None),
        )
      }

  case class FirstPage(xref: ByteVector, xrefLength: Long, data: NonEmptyList[ByteVector], firstObjNumber: Long)

  val linearizationSize: Long =
    100

  def encodeFirstPage[A]
  (trailerData: Prim.Dict, totalCount: Int)
  (headerSize: Long)
  (firstPageChunk: Chunk[Part[A]])
  : Attempt[FirstPage] =
    for {
      firstPage <- Scodec.attemptNel("first page objects")(firstPageChunk)
      firstNumber <- objectNumber(firstPage.head)
      xrefOffset = headerSize + linearizationSize
      (entries, data, trailer) <- encodeFirstPageParts(firstPage, totalCount, trailerData)
      xrefLength <- calculateXrefLength(entries.size + 1, totalCount, trailer.data)
      encXref <- WritePdf.encodeXref(entries, trailer, xrefOffset + xrefLength)
    } yield FirstPage(
      encXref,
      xrefOffset + data.map(_.size).fold + xrefLength,
      data,
      firstNumber,
    )

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

  def linearizationObj(number: Long, data: Prim): IndirectObj =
    IndirectObj(Obj(Obj.Index(number, 0), data), None)

  def createLinearizationPull(number: Long, params: LinearizationParams): Pull[IO, Nothing, ByteVector] =
    StreamUtil.attemptPull("create linearization object")(
      IndirectObj.Codec_IndirectObj.encode(linearizationObj(number, linearizationDict(params))).map(_.bytes)
    )

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

  case class FirstPageOffsets(offset: Long, startxref: Long)

  def outputFirstPage
  (trailer: Prim.Dict, totalCount: Int, headerSize: Long, fileSize: Long)
  (firstPage: Chunk[Part[Trailer]])
  : Pull[IO, ByteVector, FirstPageOffsets] = {
    val attempt = encodeFirstPage(trailer, totalCount)(headerSize)(firstPage)
    StreamUtil.attemptPullWith("encoding first page")(attempt) {
      case FirstPage(xref, xrefLength, data, firstNumber) =>
        for {
          _ <- createLinearizationPull(firstNumber - 1, linParams(totalCount, fileSize)).flatMap(Pull.output1)
          _ <- Pull.output1(xref)
          _ <- Pull.output(Chunk.seq(data.toList))
        } yield FirstPageOffsets(headerSize + data.map(_.size).fold + xrefLength, headerSize + linearizationSize)
    }
  }

  def pullFirstPage
  (trailerData: Prim.Dict, count: Int, totalCount: Int, headerSize: Long, fileSize: Long)
  (parts: Stream[IO, Part[Trailer]])
  : Pull[IO, ByteVector, Unit] =
    parts
      .pull
      .unconsN(count, false)
      .flatMap {
        case Some((firstPage, tail)) =>
          val finalTrailer = Trailer(totalCount - count, trailerData, None)
          outputFirstPage(trailerData, totalCount, headerSize, fileSize)(firstPage)
            .flatMap {
              case FirstPageOffsets(offset, _) =>
                WritePdf.pullParts(tail ++ Stream(Part.Meta(finalTrailer)))(offset)
              }
            .void
        case None =>
          StreamUtil.failPull("no objects in parts stream")
      }

  def pipe
  (trailer: Prim.Dict, firstPageCount: Int, totalCount: Int, fileSize: Long)
  : Pipe[IO, Part[Trailer], ByteVector] =
    WritePdf.consumeVersion(_)
      .flatMap {
        case (offset, tail) =>
          pullFirstPage(trailer, firstPageCount, totalCount, offset, fileSize)(tail)
      }
      .stream

  def write(pdf: LinearizedPdf): Stream[IO, ByteVector] =
    ???

  def fresh: Pipe[IO, IndirectObj, ByteVector] =
    in =>
      Stream.eval(in.compile.to(List).map(LinearizedPdf.apply))
        .flatMap {
          case Left(error) => StreamUtil.failStream(error)
          case Right(pdf) => write(pdf)
        }
}
