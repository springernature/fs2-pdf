package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._
import scodec.Attempt
import scodec.bits.ByteVector

case class XrefObjMeta(index: Obj.Index, size: Long)

sealed trait EncodedUpdate

object EncodedUpdate
{
  case class Linearized(data: ByteVector)
  extends EncodedUpdate

  case class Regular(old: ByteVector, update: ByteVector)
  extends EncodedUpdate
}

object EncodeMeta
{
  def xrefEntry(generation: Int, offset: Long): Xref.Entry =
    Xref.entry(offset, generation, Xref.EntryType.InUse)

  def objectXrefEntry: (Obj.Index, Long) => Xref.Entry = {
    case (Obj.Index(_, generation), offset) =>
      xrefEntry(generation, offset)
  }

  def objectXrefTable: (Obj.Index, Long) => Xref.Table = {
    case (Obj.Index(number, generation), offset) =>
      Xref.Table(number, List(xrefEntry(generation, offset)))
  }

  def trailerUpdate(previousSize: Long, startxref: Option[Long], newEntries: Int): Prim.Dict =
    Prim.Dict(
      Map("Size" -> Prim.num(previousSize + newEntries)) ++
      Map.from(startxref.map(sx => "Prev" -> Prim.num(sx)).toList)
    )

  def mergeTrailerDicts(update: Prim.Dict): Trailer => Prim.Dict = {
    case Trailer(_, Prim.Dict(data)) => Prim.Dict(data ++ update.data)
  }

  def trailer(previousTrailer: Trailer, previousSize: Long, offset: Option[Long], newEntries: Int): Trailer =
    Trailer(previousSize + newEntries, mergeTrailerDicts(trailerUpdate(previousSize, offset, newEntries))(previousTrailer))

  def offsets(base: Long)(sizes: List[Long]): List[Long] =
    sizes.scanLeft(base)(_ + _)

  def encode(xref: Xref): Attempt[ByteVector] =
    Xref.Codec_Xref.encode(xref)
      .map(_.bytes)

  def indexOffsets(baseOffset: Long)(objs: List[XrefObjMeta]): (List[(Obj.Index, Long)], Long) = {
    val sizes = objs.map(_.size)
    val os = offsets(baseOffset)(sizes)
    (objs.map(_.index).zip(os), baseOffset + sizes.sum)
  }

  def previousSize(xrefs: NonEmptyList[Xref]): Long =
    xrefs.foldMap(_.tables.foldMap(_.entries.size))

  def simpleXref(
    updates: List[XrefObjMeta],
    additions: List[XrefObjMeta],
    previousDocSize: Long,
    xrefs: NonEmptyList[Xref],
  ): Xref = {
    val prevOffset: Long =
      if (isLinearized(xrefs)) xrefs.head.startxref
      else xrefs.last.startxref
    val prev = previousXref(xrefs)
    val objects = updates ++ additions
    val indexes = objects.map(_.index)
    val sizes = objects.map(_.size)
    val startxref = previousDocSize + sizes.sum
    val tables =
      indexes
        .zip(offsets(previousDocSize)(sizes))
        .map(objectXrefTable.tupled)
    Xref(tables, trailer(prev.trailer, previousSize(xrefs), Some(prevOffset), additions.size), startxref)
  }

  def regular(
    updates: List[XrefObjMeta],
    additions: List[XrefObjMeta],
    previousDocSize: Long,
    last: ParsedXref,
    xrefs: NonEmptyList[Xref],
  ): Attempt[EncodedUpdate] =
    for {
      update <- encode(simpleXref(updates, additions, previousDocSize, xrefs))
    } yield EncodedUpdate.Regular(last.data, update)

  def isLinearized(xrefs: NonEmptyList[Xref]): Boolean =
    xrefs.size == 2 && xrefs.last.startxref == 0

  def previousXref(xrefs: NonEmptyList[Xref]): Xref =
    if (isLinearized(xrefs)) xrefs.last
    else xrefs.head

  def apply(
    updates: List[XrefObjMeta],
    additions: List[XrefObjMeta],
    previousDocSize: Long,
    xrefs: NonEmptyList[ParsedXref],
  ): Attempt[EncodedUpdate] =
    regular(updates, additions, previousDocSize, xrefs.head, xrefs.map(_.xref))
}
