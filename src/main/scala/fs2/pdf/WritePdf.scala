package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.ByteVector

object GenerateXref
{
  def xrefEntries(indexes: List[Obj.Index], sizes: List[Long], initialOffset: Long): List[(Long, Xref.Entry)] =
    indexes
      .zip(EncodeMeta.offsets(initialOffset)(sizes))
      .map { case (index, offset) => (index.number, EncodeMeta.objectXrefEntry(index, offset)) }
      .sortBy(_._1)

  def padEntries(entries: List[(Long, Xref.Entry)]): List[Xref.Entry] =
    entries
      .foldLeft(Vector.empty[Xref.Entry]) {
        case (z, (number, entry)) =>
          (z ++ Vector.fill(number.toInt - z.size - 1)(Xref.Entry.dummy)).appended(entry)
      }
      .toList

  def deduplicateEntries(entries: List[(Long, Xref.Entry)]): List[(Long, Xref.Entry)] =
    entries
      .foldLeft(List.empty[(Long, Xref.Entry)]) {
        case (Nil, (number, entry)) =>
          List((number, entry))
        case ((prevNumber, _) :: tail, (number, entry)) if prevNumber == number =>
          (number, entry) :: tail
        case (tail, h) =>
          h :: tail
      }
      .reverse

  def apply(meta: List[XrefObjMeta], trailerDict: Trailer, initialOffset: Long): Xref = {
    val sizes = meta.map(_.size)
    val indexes = meta.map(_.index)
    val startxref = initialOffset + sizes.sum
    val entries = padEntries(deduplicateEntries(xrefEntries(indexes, sizes, initialOffset)))
    val trailer = EncodeMeta.trailer(trailerDict, 0, None, entries.size + 1)
    Xref(List(Xref.Table(0, Xref.Entry.freeHead :: entries)), trailer, startxref)
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
      StreamUtil.attemptPull("encoding object")(EncodedObj.indirect(obj)) {
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
    StreamUtil.attemptPull("encode version")(Codecs.encodeBytes(version))(
      a => Pull.output1(a).as(a.size)
    )

  def encodeXref(entries: List[XrefObjMeta], trailer: Trailer, initialOffset: Long)
  : Attempt[ByteVector] =
    Codecs.encodeBytes(GenerateXref(entries, trailer, initialOffset))

  def writeXref(entries: List[XrefObjMeta], trailer: Trailer, initialOffset: Long)
  : Pull[IO, ByteVector, Unit] =
    StreamUtil.attemptPull("encoding xref")(encodeXref(entries, trailer, initialOffset))(Pull.output1)

  def pullParts(parts: Stream[IO, Part[Trailer]])(initialOffset: Long): Pull[IO, ByteVector, Unit] =
    StreamUtil.pullState(encode)(parts)(EncodeLog(Nil, None))
      .flatMap {
        case EncodeLog(entries, Some(trailer)) =>
          writeXref(entries.reverse, trailer, initialOffset)
        case EncodeLog(_, None) =>
          StreamUtil.failPull("no trailer in parts stream")
      }

  def parts: Pipe[IO, Part[Trailer], ByteVector] =
    _
      .pull
      .uncons1
      .flatMap {
        case Some((Part.Version(version), tail)) =>
          outputVersion(version).flatMap(pullParts(tail))
        case Some((head, tail)) =>
          outputVersion(Version.default).flatMap(pullParts(Stream(head) ++ tail))
        case None =>
          StreamUtil.failPull("no elements in parts stream")
      }
      .stream

  def objects(trailer: Trailer): Pipe[IO, IndirectObj, ByteVector] =
    in => (in.map(Part.Obj(_)) ++ Stream(Part.Meta(trailer))).through(parts)
}
