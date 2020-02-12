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
    case Part.Obj(IndirectObj(Obj.Index(n, _), _, _)) => Attempt.successful(n)
    case Part.Unparsable(Obj.Index(n, _), _) => Attempt.successful(n)
    case _ => Codecs.fail("first part is not an object")
  }

  def encode[A]
  : Part[A] => Attempt[EncodedObj] = {
    case Part.Obj(obj) =>
      EncodedObj.indirect(obj)
    case Part.Unparsable(index, data) =>
      Attempt.successful(EncodedObj(XrefObjMeta(index, data.size), data))
    case Part.Meta(_) =>
      Codecs.fail("trailer in first page data")
    case Part.Version(_) =>
      Codecs.fail("Part.Version not at the head of stream")
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
          Trailer(count, trailer ++ Prim.dict("Size" -> Prim.num(count))),
        )
      }

  case class FirstPage(xref: ByteVector, xrefLength: Long, encLin: ByteVector, data: NonEmptyList[ByteVector])

  def encodeFirstPage[A]
  (linearization: Prim.Dict, trailerData: Prim.Dict, totalCount: Int)
  (headerSize: Long)
  (firstPageChunk: Chunk[Part[A]])
  : Attempt[FirstPage] =
    for {
      firstPage <- Codecs.attemptNel("first page objects")(firstPageChunk)
      firstNumber <- objectNumber(firstPage.head)
      encLin <- EncodedObj.indirect(IndirectObj.nostream(firstNumber - 1, linearization))
      xrefOffset = headerSize + encLin.xref.size
      (entries, data, trailer) <- encodeFirstPageParts(firstPage, totalCount, trailerData)
      xrefLength <- calculateXrefLength(entries.size + 1, totalCount, trailer.data)
      encXref <- WritePdf.encodeXref(entries, trailer, xrefOffset + xrefLength)
    } yield FirstPage(
      encXref,
      xrefOffset + data.map(_.size).fold + xrefLength,
      encLin.bytes,
      data,
    )

  case class FirstPageOffsets(offset: Long, startxref: Long)

  def outputFirstPage
  (linearization: Prim.Dict, trailer: Prim.Dict, totalCount: Int, headerSize: Long)
  (firstPage: Chunk[Part[Trailer]])
  : Pull[IO, ByteVector, FirstPageOffsets] = {
    val attempt = encodeFirstPage(linearization, trailer, totalCount)(headerSize)(firstPage)
    StreamUtil.attemptPullWith("encoding first page")(attempt) {
      case FirstPage(xref, xrefLength, encLin, data) =>
        for {
          _ <- Pull.output1(encLin)
          _ <- Pull.output1(xref)
          _ <- Pull.output(Chunk.seq(data.toList))
        } yield FirstPageOffsets(headerSize + data.map(_.size).fold + xrefLength, headerSize + encLin.size)
    }
  }

  def pullFirstPage
  (linearization: Prim.Dict, trailerData: Prim.Dict, count: Int, totalCount: Int, headerSize: Long)
  (parts: Stream[IO, Part[Trailer]])
  : Pull[IO, ByteVector, Unit] =
    parts
      .pull
      .unconsN(count, false)
      .flatMap {
        case Some((firstPage, tail)) =>
          val finalTrailer = Trailer(totalCount - count, trailerData)
          outputFirstPage(linearization, trailerData, totalCount, headerSize)(firstPage)
            .flatMap {
              case FirstPageOffsets(offset, _) =>
                WritePdf.pullParts(tail ++ Stream(Part.Meta(finalTrailer)))(offset)
              }
            .void
        case None =>
          StreamUtil.failPull("no objects in parts stream")
      }

  def pipe
  (linearization: Prim.Dict, trailer: Prim.Dict, firstPageCount: Int, totalCount: Int)
  : Pipe[IO, Part[Trailer], ByteVector] =
    WritePdf.consumeVersion(_)
      .flatMap {
        case (offset, tail) =>
          pullFirstPage(linearization, trailer, firstPageCount, totalCount, offset)(tail)
      }
      .stream
}
