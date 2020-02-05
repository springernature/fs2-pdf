package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.bits.BitVector

object Generate
{
  def generateXrefEntry(start: Long)(index: Obj.Index): (Long, Xref.Entry) =
    (index.number, EncodeMeta.xrefEntry(index.generation, start))

  def generateXref(trailer: Prim.Dict, startxref: Long, entries: List[(Long, Xref.Entry)]): Xref = {
    val sorted = entries.sortBy(_._1)
    val free = Xref.entry(0, 65535, Xref.EntryType.Free)
    val tables = List(Xref.Table(0, free :: sorted.map(_._2)))
    Xref(tables, Trailer(sorted.size + 1, trailer), startxref)
  }

  def encodeWithXref
  (trailer: Prim.Dict, start: Long)
  (entries: List[(Long, Xref.Entry)])
  (in: Stream[IO, IndirectObj])
  : Pull[IO, BitVector, Unit] =
    in
      .pull
      .uncons1
      .flatMap {
        case Some((obj, tail)) =>
          StreamUtil.attemptPull("failed to encode object")(Codecs.encode(obj))(
            bits =>
              Pull.output1(bits) >>
              encodeWithXref(trailer, start + bits.bytes.size)(generateXrefEntry(start)(obj.index) :: entries)(tail)
          )
        case None =>
          val encodeResult = Codecs.encode(generateXref(trailer, start, entries))
          StreamUtil.attemptPull("failed to encode xref")(encodeResult)(Pull.output1)
      }

  def header: String =
    "%PDF-1.4\n%âãÏÓ\n"

  def apply(trailer: Prim.Dict): Pipe[IO, IndirectObj, Byte] =
    in =>
      StreamUtil.string(header) ++
      in
        .through(encodeWithXref(trailer, header.getBytes.size)(Nil)(_).stream)
        .through(StreamUtil.bitsPipe)
}
