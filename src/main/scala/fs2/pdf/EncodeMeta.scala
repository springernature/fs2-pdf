package fs2
package pdf

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
}
