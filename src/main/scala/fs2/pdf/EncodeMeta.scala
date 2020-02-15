package fs2
package pdf

import cats.data.NonEmptyList

case class XrefObjMeta(index: Obj.Index, size: Long)

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
      Xref.Table(number, NonEmptyList.one(xrefEntry(generation, offset)))
  }

  def trailerUpdate(previousSize: Long, startxref: Option[Long], newEntries: Int): Prim.Dict =
    Prim.Dict(
      Map("Size" -> Prim.num(previousSize + newEntries)) ++
      Map.from(startxref.map(sx => "Prev" -> Prim.num(sx)).toList)
    )

  def mergeTrailerDicts(update: Prim.Dict): Trailer => Prim.Dict = {
    case Trailer(_, Prim.Dict(data), _) => Prim.Dict(data ++ update.data)
  }

  def trailer(previousTrailer: Trailer, previousSize: Long, offset: Option[Long], newEntries: Int): Trailer =
    Trailer(
      previousSize + newEntries,
      mergeTrailerDicts(trailerUpdate(previousSize, offset, newEntries))(previousTrailer),
      previousTrailer.root,
    )

  def offsets(base: Long)(sizes: NonEmptyList[Long]): NonEmptyList[Long] =
    NonEmptyList(base, sizes.tail.scanLeft(base + sizes.head)(_ + _))
}
