package fs2
package pdf

import cats.data.NonEmptyList

/**
  * Data used for generating an xref entry for an indirect object.
  * Can only be used in combination with other objects, since the xref stores the byte offset, which needs to be
  * calcuclated from the sizes of the previous objects.
  *
  * @param index the number and generation of the object
  * @param size the size in bytes of the encoded object
  */
case class XrefObjMeta(index: Obj.Index, size: Long)

/**
  * Utilities for generating xref entries
  */
object EncodeMeta
{
  /**
    * Constructor for regular (non-deleted) xref entries
    *
    * @param offset the byte offset of the associated object in the file
    * @param generation the generation number indicates that the object replaces another, unrelated and deleted, object
    * @return an xref entry
    */
  def xrefEntry(offset: Long, generation: Int): Xref.Entry =
    Xref.entry(offset, generation, Xref.EntryType.InUse)

  private[pdf]
  def objectXrefEntry: (Obj.Index, Long) => Xref.Entry = {
    case (Obj.Index(_, generation), offset) =>
      xrefEntry(offset, generation)
  }

  private[pdf]
  def objectXrefTable: (Obj.Index, Long) => Xref.Table = {
    case (Obj.Index(number, generation), offset) =>
      Xref.Table(number, NonEmptyList.one(xrefEntry(offset, generation)))
  }

  private[pdf]
  def trailerUpdate(previousSize: Long, newEntries: Int): Prim.Dict =
    Prim.dict("Size" -> Prim.num(previousSize + newEntries))

  private[pdf]
  def mergeTrailerDicts(update: Prim.Dict): Trailer => Prim.Dict = {
    case Trailer(_, Prim.Dict(data), _) => Prim.Dict(data ++ update.data)
  }

  /**
    * Construct a new trailer from an existing trailer, with added entries.
    *
    * Updates the Size entry in the trailer.
    *
    * @param previousTrailer trailer from a file that was modified
    * @param previousSize size entry from the old trailer
    * @param newEntries number of objects that were added to the original pdf
    * @return new trailer
    */
  def trailer(previousTrailer: Trailer, previousSize: Long, newEntries: Int): Trailer =
    Trailer(
      previousSize + newEntries,
      mergeTrailerDicts(trailerUpdate(previousSize, newEntries))(previousTrailer),
      previousTrailer.root,
    )

  /**
    * Aggregate the byte offsets of a list of objects from their sizes.
    *
    * @param base the byte offset of the first object
    * @param sizes the sizes in bytes of the encoded objects
    * @return the byte offsets of the objects
    */
  def offsets(base: Long)(sizes: NonEmptyList[Long]): NonEmptyList[Long] =
    NonEmptyList(base, sizes.tail.scanLeft(base + sizes.head)(_ + _))
}
