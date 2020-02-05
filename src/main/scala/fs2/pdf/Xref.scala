package fs2
package pdf

import scodec.{Attempt, Codec, Err}
import scodec.bits.ByteVector

case class Trailer(size: BigDecimal, data: Prim.Dict)

case class Xref(tables: List[Xref.Table], trailer: Trailer, startxref: Long)

object Xref
extends XrefCodec
{
  case class Table(offset: Long, entries: List[Xref.Entry])

  case class Entry(index: Index, `type`: EntryType)

  object Entry
  {
    def freeHead: Entry =
      entry(0, 65535, EntryType.Free)

    def dummy: Entry =
      entry(0, 0, EntryType.InUse)
  }

  sealed trait Index

  object Index
  {
    case class Regular(offset: String, generation: String)
    extends Index

    case class Compressed(obj: Long, index: Int)
    extends Index
  }

  sealed trait EntryType

  object EntryType
  {
    case object InUse
    extends EntryType

    case object Free
    extends EntryType

    def to: EntryType => Attempt[Byte] = {
      case EntryType.InUse => Attempt.successful('n')
      case EntryType.Free => Attempt.successful('f')
    }

    def from: Byte => Attempt[EntryType] = {
      case 'n' => Attempt.successful(EntryType.InUse)
      case 'f' => Attempt.successful(EntryType.Free)
      case a => Attempt.failure(Err(s"invalid xref entry type byte `$a`"))
    }
  }

  def padZeroes[A: Numeric](max: Int)(number: A): String = {
    val numberString = number.toString
    val padding = new String(Array.fill(max - numberString.length)('0'))
    padding + numberString
  }

  def entry(offset: Long, generation: Int, `type`: EntryType): Xref.Entry =
    Xref.Entry(Index.Regular(padZeroes(10)(offset), padZeroes(5)(generation)), `type`)

  def compressed(obj: Long, index: Int, `type`: EntryType): Xref.Entry =
    Xref.Entry(Index.Compressed(obj, index), `type`)
}

case class ParsedXref(xref: Xref, data: ByteVector)

trait XrefCodec
{
  import scodec.codecs._

  def offset: Codec[String] =
    Codecs.stringOf(10).withContext("offset") <~ Codecs.space.withContext("offset space")

  def generation: Codec[String] =
    Codecs.stringOf(5).withContext("generation") <~ Codecs.space.withContext("generation space")

  implicit def regularIndex: Codec[Xref.Index.Regular] =
    (Codecs.productCodec(offset ~ generation): Codec[Xref.Index.Regular])
      .withContext("regular index")

  implicit def compressedIndex: Codec[Xref.Index.Compressed] =
    (Codecs.productCodec(Codecs.ascii.long ~ Codecs.ascii.int): Codec[Xref.Index.Compressed])
      .withContext("compressed index")

  def index: Codec[Xref.Index] =
    Codec.coproduct[Xref.Index].choice

  def entryType: Codec[Xref.EntryType] =
    byte.exmap(Xref.EntryType.from, Xref.EntryType.to)

  def twoByteNewline: Codec[Unit] =
    choice(
      Codecs.space.withContext("end space") <~ Codecs.linuxNewline.withContext("end newline"),
      Codecs.windowsNewline.withContext("end newline"),
    )

  def entry: Codec[Xref.Entry] =
    Codecs.productCodec(index ~ entryType <~ twoByteNewline)

  def range: Codec[(Long, Int)] =
    (Codecs.ascii.long.withContext("offset") <~ Codecs.ws) ~ Codecs.ascii.int.withContext("size") <~ Codecs.newline

  def validateTrailer(data: Prim.Dict): Attempt[Trailer] =
    Prim.Dict.path("Size")(data) {
      case Prim.Number(size) => Trailer(size, data)
    }

  def trailerDict: Codec[Trailer] =
    Prim.Codec_Dict
      .withContext("trailer")
      .exmap(validateTrailer, a => Attempt.successful(a.data))

  def trailerKw: Codec[Unit] =
    Codecs.constantLine("trailer")

  def Codec_Trailer: Codec[Trailer] =
    trailerKw ~> trailerDict <~ Codecs.nlWs

  def startxref: Codec[Long] =
    Codecs.constantLine("startxref") ~> Codecs.ascii.long.withContext("startxref") <~ Codecs.newline

  def table: Codec[Xref.Table] =
    range.withContext("range")
      .flatZip { case (_, size) => listOfN(provide(size), entry).withContext("entries") }
      .withContext("table")
      .xmap(
        { case ((o, _), es) => Xref.Table(o, es) },
        t => ((t.offset, t.entries.size), t.entries),
      )

  implicit def Codec_Xref: Codec[Xref] =
    Codecs.productCodec(
      Codecs.constantLine("xref") ~> Codecs.manyTillCodec(trailerKw)(table).withContext("xref tables") ~
      Codec_Trailer ~
      startxref <~
      Codecs.str("%%EOF") <~ optional(bitsRemaining, Codecs.nlWs).unit(Some(()))
    )
}
