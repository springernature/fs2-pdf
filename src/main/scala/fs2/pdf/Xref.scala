package fs2
package pdf

import cats.data.NonEmptyList
import cats.implicits._
import codec.{Many, Newline, Text, Whitespace}
import scodec.{Attempt, Codec, Err}

case class Trailer(size: BigDecimal, data: Prim.Dict, root: Option[Prim.Ref])

object Trailer
{
  def sanitize(trailers: NonEmptyList[Trailer]): Trailer =
    Trailer(
      trailers.toList.maxByOption(_.size).getOrElse(trailers.head).size,
      Prim.Dict(
        trailers.map(_.data.data).reduceLeft((a, b) => a ++ b)
          .removed("Prev")
          .removed("DecodeParms")
      ),
      trailers.head.root,
    )

  def fromData(data: Prim.Dict): Attempt[Trailer] =
    Prim.Dict.path("Size")(data) {
      case Prim.Number(size) =>
        val root = Prim.Dict.path("Root")(data) { case ref @ Prim.Ref(_, _) => ref }.toOption
        Trailer(size, data, root)
    }
}

case class Xref(tables: NonEmptyList[Xref.Table], trailer: Trailer, startxref: Long)

object Xref
extends XrefCodec
{
  case class Table(offset: Long, entries: NonEmptyList[Xref.Entry])

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

trait XrefCodec
{
  import scodec.codecs.{choice, listOfN, provide, optional, bitsRemaining}
  import Newline.{lf, crlf, newline}
  import Whitespace.{nlWs, ws, space, skipWs}
  import Text.{stringOf, ascii, str}

  def offset: Codec[String] =
    stringOf(10).withContext("offset") <~ space.withContext("offset space")

  def generation: Codec[String] =
    stringOf(5).withContext("generation") <~ space.withContext("generation space")

  implicit def regularIndex: Codec[Xref.Index.Regular] =
    (offset :: generation)
      .as[Xref.Index.Regular]
      .withContext("regular index")

  implicit def compressedIndex: Codec[Xref.Index.Compressed] =
    (ascii.long :: ascii.int)
      .as[Xref.Index.Compressed]
      .withContext("compressed index")

  def index: Codec[Xref.Index] =
    Codec.coproduct[Xref.Index].choice

  def entryType: Codec[Xref.EntryType] =
    scodec.codecs.byte.exmap(Xref.EntryType.from, Xref.EntryType.to)

  def twoByteNewline: Codec[Unit] =
    choice(
      space.withContext("end space") <~ lf.withContext("end newline"),
      crlf.withContext("end newline"),
    )

  def entry: Codec[Xref.Entry] =
    (index :: entryType <~ twoByteNewline).as[Xref.Entry]

  def range: Codec[(Long, Int)] =
    (ascii.long.withContext("offset") <~ ws) ~ ascii.int.withContext("size") <~ newline

  def trailerDict: Codec[Trailer] =
    Prim.Codec_Dict
      .withContext("trailer")
      .exmap(Trailer.fromData, a => Attempt.successful(a.data))

  def trailerKw: Codec[Unit] =
    str("trailer") <~ nlWs

  def Codec_Trailer: Codec[Trailer] =
    trailerKw ~> trailerDict <~ nlWs

  def startxref: Codec[Long] =
    (str("startxref") ~> nlWs ~> ascii.long.withContext("startxref offset") <~ nlWs)
      .withContext("startxref")

  def table: Codec[Xref.Table] =
    range.withContext("range")
      .flatZip { case (_, size) => listOfN(provide(size), entry).withContext("entries") }
      .withContext("table")
      .exmap(
        { case ((o, _), es) => Scodec.attemptNel("xref table entries")(es).map(e => Xref.Table(o, e)) },
        t => Attempt.successful(((t.offset, t.entries.size), t.entries.toList)),
      )

  def tables: Codec[NonEmptyList[Xref.Table]] =
    Many.tillDecodes1(trailerKw)(table).withContext("xref tables")

  def eof: Codec[Unit] =
    str("%%EOF") <~ optional(bitsRemaining, nlWs).unit(Some(()))

  implicit def Codec_Xref: Codec[Xref] =
    (
      (str("xref") ~> nlWs) ~>
      tables ::
      (skipWs ~> Codec_Trailer) ::
      startxref <~
      eof
    ).as[Xref]
}

case class StartXref(offset: Long)

object StartXref
{
  import Text.{str, ascii}
  import Whitespace.nlWs

  implicit def Codec_StartXref: Codec[StartXref] =
    (str("startxref") ~> nlWs ~> ascii.long.withContext("startxref offset") <~ nlWs)
      .withContext("startxref")
      .as[StartXref]
}
