package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.ByteVector

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

  def padEntries(entries: NonEmptyList[(Long, Xref.Entry)]): NonEmptyList[Xref.Entry] = {
    entries
      .reduceLeftTo(a => NonEmptyList.one(a)) {
        case (z @ NonEmptyList((prevNumber, _), _), (number, entry)) =>
          val padding = List.fill((number - prevNumber - 1).toInt)((0L, Xref.Entry.dummy))
          NonEmptyList((number, entry), padding) ::: z
      }
      .reverse
      .map(_._2)
  }

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

object WritePdf
{
  case class EncodeLog(entries: List[XrefObjMeta], trailer: Option[Trailer])
  {
    def entry(newEntry: XrefObjMeta): EncodeLog =
      copy(entries = newEntry :: entries)
  }

  def encode(state: EncodeLog)
  : Part[Trailer] => Pull[IO, ByteVector, EncodeLog] = {
    case Part.Obj(obj) =>
      StreamUtil.attemptPullWith("encoding object")(EncodedObj.indirect(obj)) {
        case EncodedObj(entry, bytes) =>
          Pull.output1(bytes).as(state.entry(entry))
      }
    case Part.Unparsable(index, data) =>
      Pull.output1(data).as(state.entry(XrefObjMeta(index, data.size)))
    case Part.Meta(trailer) =>
      Pull.pure(state.copy(trailer = Some(trailer)))
    case Part.Version(_) =>
      StreamUtil.failPull("Part.Version not at the head of stream")
  }

  def outputVersion(version: Version): Pull[IO, ByteVector, Long] =
    StreamUtil.attemptPullWith("encode version")(Codecs.encodeBytes(version))(a => Pull.output1(a).as(a.size))

  def encodeXref(
    entries: NonEmptyList[XrefObjMeta],
    trailer: Trailer,
    initialOffset: Long,
  )
  : Attempt[ByteVector] =
    Codecs.encodeBytes(GenerateXref(entries, trailer, initialOffset))

  def writeXref(
    entries: NonEmptyList[XrefObjMeta],
    trailer: Trailer,
    initialOffset: Long,
  ): Pull[IO, ByteVector, Unit] = {
    val attempt = encodeXref(entries, trailer, initialOffset)
    StreamUtil.attemptPullWith("encoding xref")(attempt)(Pull.output1)
  }

  def pullParts
  (parts: Stream[IO, Part[Trailer]])
  (initialOffset: Long)
  : Pull[IO, ByteVector, Long] =
    StreamUtil.pullState(encode)(parts)(EncodeLog(Nil, None))
      .flatMap {
        case EncodeLog(h :: t, Some(trailer)) =>
          writeXref(NonEmptyList(h, t).reverse, trailer, initialOffset).as(0L)
        case EncodeLog(Nil, _) =>
          StreamUtil.failPull("no xref entries in parts stream")
        case EncodeLog(_, None) =>
          StreamUtil.failPull("no trailer in parts stream")
      }

  def consumeVersion(parts: Stream[IO, Part[Trailer]]): Pull[IO, ByteVector, (Long, Stream[IO, Part[Trailer]])] =
    parts
      .pull
      .uncons1
      .flatMap {
        case Some((Part.Version(version), tail)) =>
          outputVersion(version).map((_, tail))
        case Some((head, tail)) =>
          outputVersion(Version.default).map((_, Stream(head) ++ tail))
        case None =>
          StreamUtil.failPull("no elements in parts stream")
      }

  def parts: Pipe[IO, Part[Trailer], ByteVector] =
    consumeVersion(_)
      .flatMap { case (offset, tail) => pullParts(tail)(offset) }
      .void
      .stream

  def objects(trailer: Trailer): Pipe[IO, IndirectObj, ByteVector] =
    in => (in.map(Part.Obj(_)) ++ Stream(Part.Meta(trailer))).through(parts)

}
