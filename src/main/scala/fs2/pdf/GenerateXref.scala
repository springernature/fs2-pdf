package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._

/**
  * Generate a cross-reference table from a list of object indexes and byte offsets, deduplicating and padding.
  */
object GenerateXref
{
  private[pdf]
  def xrefEntries(indexes: NonEmptyList[Obj.Index], sizes: NonEmptyList[Long], initialOffset: Long)
  : NonEmptyList[(Long, Xref.Entry)] =
    indexes
      .zipWith(EncodeMeta.offsets(initialOffset)(sizes)) {
        case (index, offset) =>
          (index.number, EncodeMeta.objectXrefEntry(index, offset))
      }
      .sortBy(_._1)

  private[pdf]
  def padEntries(entries: NonEmptyList[(Long, Xref.Entry)]): NonEmptyList[Xref.Entry] =
    entries
      .reduceLeftTo(a => NonEmptyList.one(a)) {
        case (z @ NonEmptyList((prevNumber, _), _), (number, entry)) =>
          val padding = List.fill((number - prevNumber - 1).toInt)((0L, Xref.Entry.dummy))
          NonEmptyList((number, entry), padding) ::: z
      }
      .reverse
      .map(_._2)

  private[pdf]
  def deduplicateEntries(entries: NonEmptyList[(Long, Xref.Entry)]): NonEmptyList[(Long, Xref.Entry)] =
    entries
      .reduceLeftTo(NonEmptyList.one(_)) {
        case (NonEmptyList((prevNumber, _), tail), (number, entry)) if prevNumber == number =>
          NonEmptyList((number, entry), tail)
        case (tail, h) =>
          h :: tail
      }
      .reverse

  /**
    * @param meta indexes and byte offsets of referenced objects
    * @param trailerDict trailer that will be amended with the size of the xref
    * @param initialOffset the number of bytes in the document before the first referenced object, like the version
    * header
    * @return a deduplicated, padded, consecutive [[Xref]]
    */
  def apply(
    meta: NonEmptyList[XrefObjMeta],
    trailerDict: Trailer,
    initialOffset: Long,
  ): Xref = {
    val indexes = meta.map(_.index)
    val sizes = meta.map(_.size)
    val startxref = initialOffset + sizes.toList.sum
    val entries = padEntries(deduplicateEntries(xrefEntries(indexes, sizes, initialOffset)))
    val firstNumber = meta.minimumBy(_.index.number).index.number
    val fromZero = firstNumber == 1L
    val zeroIncrement = if (fromZero) 1 else 0
    val trailer = EncodeMeta.trailer(trailerDict, 0, entries.size + zeroIncrement)
    val withFree =
      if (fromZero) Xref.Entry.freeHead :: entries
      else entries
    val tableOffset = if (fromZero) 0L else firstNumber
    Xref(NonEmptyList.one(Xref.Table(tableOffset, withFree)), trailer, StartXref(startxref))
  }
}
