package fs2
package pdf

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.BitVector
import scodec.interop.cats.{AttemptMonadErrorInstance, DecoderMonadInstance}

case class XrefStream(tables: List[Xref.Table], trailer: Trailer)

object XrefStream
{
  def xrefStreamOffsets(data: Prim.Dict): Attempt[List[(BigDecimal, BigDecimal)]] = {
    Prim.Dict.number("Size")(data)
      .flatMap { size =>
        Prim.Dict.numbers("Index")(data)
          .flatMap { index =>
            index.sliding(2, 2).toList.foldM(List.empty[(BigDecimal, BigDecimal)]) {
              case (z, List(offset, size)) =>
                Attempt.successful((offset, size) :: z)
              case _ =>
                Codecs.fail("broken xref stream index")
            }
          }
          .recoverWith { case _ => Attempt.successful(List((0, size))) }
    }
  }

  def xrefStreamFieldWidths
  (size: Long)
  (offsets: List[(BigDecimal, BigDecimal)])
  (data: Prim.Dict)
  : Attempt[(BigDecimal, BigDecimal, BigDecimal)] = {
    val entrySize = size / offsets.map(_._2).sum
    Prim.Dict.numbers("W")(data)
      .flatMap {
        case List(width1, width2, width3) if entrySize == (width1 + width2 + width3) =>
          Attempt.successful((if (width1 == 0) 1 else width1, width2, width3))
        case a =>
          Codecs.fail(s"invalid xref stream field widths: $a (size: $size, entry size: $entrySize)")
    }
  }

  def decodeXrefStream(data: Prim.Dict, stream: BitVector)
  : Attempt[(BigDecimal, List[Xref.Table])] =
    for {
      offsets <- xrefStreamOffsets(data)
      (width1, width2, width3) <- xrefStreamFieldWidths(stream.bytes.size)(offsets)(data)
      DecodeResult(tables, _) <- XrefStreamCodec.decoder(offsets, width1, width2, width3).decode(stream)
    } yield (tables.foldMap(_.entries.size), tables)

  val unwantedTrailerKeys: List[String] =
    List("W", "Index", "Filter", "Length", "Type", "Size", "Prev")

  def cleanTrailer(data: Prim.Dict): Prim.Dict =
    Prim.Dict(
      data
        .data
        .view
        .filterKeys(data => !unwantedTrailerKeys.contains(data))
        .toMap
    )

  def trailerSize(data: Prim.Dict): Option[BigDecimal] =
    Prim.Dict.path("Size")(data) { case Prim.Number(size) => size }.toOption

  def apply(data: Prim.Dict)(stream: Parsed.Stream): Attempt[XrefStream] =
    for {
      uncompressed <- stream.data.value
      (entryCount, tables) <- decodeXrefStream(data, uncompressed)
    } yield XrefStream(tables, Trailer(trailerSize(data).getOrElse(entryCount), cleanTrailer(data)))
}

object XrefStreamCodec
{
  import scodec.codecs._

  def parseEntry: ((Short, Long), Int) => Option[Xref.Entry] = {
    case ((0, nextFree), generation) => Some(Xref.entry(nextFree, generation, Xref.EntryType.Free))
    case ((1, offset), generation) => Some(Xref.entry(offset, generation, Xref.EntryType.InUse))
    case ((2, number), index) => Some(Xref.compressed(number, index, Xref.EntryType.InUse))
    case _ => None
  }

  def entryField[A](num: Int, width: BigDecimal)(dec: Codec[A]): Codec[A] =
    dec.withContext(s"xref entry field $num ($width bytes)")

  def entryDecoder(width1: BigDecimal, width2: BigDecimal, width3: BigDecimal): Decoder[((Short, Long), Int)] =
    entryField(1, width1)(ushort(width1.toInt * 8)) ~
    entryField(2, width2)(ulong(width2.toInt * 8)) ~
    entryField(3, width3)(uint(width3.toInt * 8))

  def tableDecoder(width1: BigDecimal, width2: BigDecimal, width3: BigDecimal)
  : (BigDecimal, BigDecimal) => Decoder[Xref.Table] =
    (offset, size) =>
      listOfN(provide(size.toInt), entryDecoder(width1, width2, width3).map(parseEntry).decodeOnly)
        .map(_.flatten)
        .map(Xref.Table(offset.toLong, _))

  def decoder(
    offsets: List[(BigDecimal, BigDecimal)],
    width1: BigDecimal,
    width2: BigDecimal,
    width3: BigDecimal,
  ): Decoder[List[Xref.Table]] =
    offsets.traverse(tableDecoder(width1, width2, width3))
}
