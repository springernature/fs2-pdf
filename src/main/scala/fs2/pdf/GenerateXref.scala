package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._

object GenerateXref
{
  def xrefEntries(indexes: NonEmptyList[Obj.Index], sizes: NonEmptyList[Long], initialOffset: Long)
  : NonEmptyList[(Long, Xref.Entry)] =
    indexes
      .zipWith(EncodeMeta.offsets(initialOffset)(sizes)) {
        case (index, offset) =>
          (index.number, EncodeMeta.objectXrefEntry(index, offset))
      }
      .sortBy(_._1)

  def padEntries(entries: NonEmptyList[(Long, Xref.Entry)]): NonEmptyList[Xref.Entry] =
    entries
      .reduceLeftTo(a => NonEmptyList.one(a)) {
        case (z @ NonEmptyList((prevNumber, _), _), (number, entry)) =>
          val padding = List.fill((number - prevNumber - 1).toInt)((0L, Xref.Entry.dummy))
          NonEmptyList((number, entry), padding) ::: z
      }
      .reverse
      .map(_._2)

  def deduplicateEntries(entries: NonEmptyList[(Long, Xref.Entry)]): NonEmptyList[(Long, Xref.Entry)] =
    entries
      .reduceLeftTo(NonEmptyList.one(_)) {
        case (NonEmptyList((prevNumber, _), tail), (number, entry)) if prevNumber == number =>
          NonEmptyList((number, entry), tail)
        case (tail, h) =>
          h :: tail
      }
      .reverse

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
    val trailer = EncodeMeta.trailer(trailerDict, 0, None, entries.size + zeroIncrement)
    val withFree =
      if (fromZero) Xref.Entry.freeHead :: entries
      else entries
    val tableOffset = if (fromZero) 0L else firstNumber
    Xref(NonEmptyList.one(Xref.Table(tableOffset, withFree)), trailer, startxref)
  }
}
