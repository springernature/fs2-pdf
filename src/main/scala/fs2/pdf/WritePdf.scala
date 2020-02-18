package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import codec.Codecs
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.ByteVector

/**
  * This provides [[Pipe]]s that turn a [[Stream]] of either [[Part]] or [[IndirectObj]] into a [[Stream]] of
  * [[ByteVector]] by encoding all of the objects, collecting their metadata and then generating a cross reference
  * table.
  *
  * @example {{{
  * pdfBytes
  *   .through(PdfStream.decode(Log.noop))
  *   .through(Parsed.indirectObjs)
  *   .through(WritePdf.objects(Trailer(Prim.Dict.empty)))
  * }}}
  */
object WritePdf
{
  private[pdf]
  case class EncodeLog(entries: List[XrefObjMeta], trailer: Option[Trailer])
  {
    def entry(newEntry: XrefObjMeta): EncodeLog =
      copy(entries = newEntry :: entries)
  }

  private[pdf]
  def encode(state: EncodeLog)
  : Part[Trailer] => Pull[IO, ByteVector, EncodeLog] = {
    case Part.Obj(obj) =>
      StreamUtil.attemptPullWith("encoding object")(EncodedObj.indirect(obj)) {
        case EncodedObj(entry, bytes) =>
          Pull.output1(bytes).as(state.entry(entry))
      }
    case Part.Meta(trailer) =>
      Pull.pure(state.copy(trailer = Some(trailer)))
    case Part.Version(_) =>
      StreamUtil.failPull("Part.Version not at the head of stream")
  }

  private[pdf]
  def outputVersion(version: Version): Pull[IO, ByteVector, Long] =
    StreamUtil.attemptPullWith("encode version")(Codecs.encodeBytes(version))(a => Pull.output1(a).as(a.size))

  private[pdf]
  def encodeXref(
    entries: NonEmptyList[XrefObjMeta],
    trailer: Trailer,
    initialOffset: Long,
  )
  : Attempt[ByteVector] =
    Codecs.encodeBytes(GenerateXref(entries, trailer, initialOffset))

  private[pdf]
  def writeXref(
    entries: NonEmptyList[XrefObjMeta],
    trailer: Trailer,
    initialOffset: Long,
  ): Pull[IO, ByteVector, Unit] = {
    val attempt = encodeXref(entries, trailer, initialOffset)
    StreamUtil.attemptPullWith("encoding xref")(attempt)(Pull.output1)
  }

  private[pdf]
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

  private[pdf]
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

  /**
    * The main pipe for encoding a PDF from its [[Part]]s.
    *
    * This requires that a [[Part.Version]] variant is at the head of the stream or doesn't occur at all, in which case
    * a default version will be used.
    * Also required is that at least one [[Part.Meta]] variant occurs in the stream, containing the necessary metadata.
    * At least one [[Part.Obj]] must be in the stream.
    *
    * @return a [[Pipe]] tranforming [[Part]] to [[ByteVector]] by encoding the objects and generating an xref.
    */
  def parts: Pipe[IO, Part[Trailer], ByteVector] =
    consumeVersion(_)
      .flatMap { case (offset, tail) => pullParts(tail)(offset) }
      .void
      .stream

  /**
    * Convenience method for raw [[IndirectObj]] streams.
    *
    * @param trailer additional data for the trailer, will be amended by the xref generator.
    * @return a [[Pipe]] tranforming [[IndirectObj]] to [[ByteVector]] by encoding the objects and generating an xref.
    */
  def objects(trailer: Trailer): Pipe[IO, IndirectObj, ByteVector] =
    in => (in.map(Part.Obj(_)) ++ Stream(Part.Meta(trailer))).through(parts)

}
