package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}

case class MediaBox(x: BigDecimal, y: BigDecimal, w: BigDecimal, h: BigDecimal)

case class Page(index: Obj.Index, data: Prim.Dict, mediaBox: MediaBox)

object Page
{
  def fromData(index: Obj.Index): Prim => Attempt[Page] = {
    case Prim.tpe("Page", data) =>
      Prim.Dict.path("MediaBox")(data) {
        case Prim.Array(List(Prim.Number(x), Prim.Number(y), Prim.Number(w), Prim.Number(h))) =>
          Page(index, data, MediaBox(x, y, w, h))
      }
    case _ =>
      Scodec.fail("not a Page object")
  }

  object obj
  {
    def unapply(obj: IndirectObj): Option[Page] =
      fromData(obj.obj.index)(obj.obj.data).toOption
  }
}

case class Pages(index: Obj.Index, data: Prim.Dict, kids: NonEmptyList[Prim.Ref], root: Boolean)

object Pages
{
  def fromData(index: Obj.Index): Prim => Attempt[Pages] = {
    case Prim.tpe("Pages", data) =>
      Prim.Dict.path("Kids")(data) {
        case Prim.refs(kids) => Pages(index, data, kids, !data.data.contains("Parent"))
      }
    case _ =>
      Scodec.fail("not a Pages object")
  }

  object obj
  {
    def unapply(obj: IndirectObj): Option[Pages] =
      fromData(obj.obj.index)(obj.obj.data).toOption
  }
}

case class FontResource(index: Obj.Index, data: Prim.Dict)

case class IndirectArray(index: Obj.Index, data: Prim.Array)

case class Image(data: Prim.Dict, stream: Uncompressed, codec: Image.Codec)

object Image
{
  sealed trait Codec

  object Codec
  {
    case object Jpg
    extends Codec

    case object Ccitt
    extends Codec

    def extension: Codec => String = {
      case Jpg => "jpg"
      case Ccitt => "tiff"
    }
  }
}

sealed trait Element

object Element
{
  sealed trait DataKind

  object DataKind
  {
    case object General
    extends DataKind

    case class Page(page: pdf.Page)
    extends DataKind

    case class Pages(pages: pdf.Pages)
    extends DataKind

    case class Array(data: Prim.Array)
    extends DataKind

    case class FontResource(res: pdf.FontResource)
    extends DataKind

    case class Info(data: Prim.Dict)
    extends DataKind
  }

  case class Data(obj: Obj, kind: DataKind)
  extends Element

  sealed trait ContentKind

  object ContentKind
  {
    case object General
    extends ContentKind

    case class Image(image: pdf.Image)
    extends ContentKind
  }

  case class Content(obj: Obj, rawStream: BitVector, stream: Uncompressed, kind: ContentKind)
  extends Element

  case class Meta(trailer: Option[Trailer], version: Option[Version])
  extends Element

  object obj
  {
    def unapply(element: Element): Option[IndirectObj] =
      element match {
        case Data(obj, _) => Some(IndirectObj(obj, None))
        case Content(obj, stream, _, _) => Some(IndirectObj(obj, Some(stream)))
        case Meta(_, _) => None
      }
  }

  def part: RewriteState[Unit] => Element => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case obj(obj) =>
        (List(Part.Obj(obj)), state)
      case Element.Meta(trailer, _) =>
        (Nil, state.copy(trailer = trailer))
    }

  def parts: Pipe[IO, Element, Part[Trailer]] =
    Rewrite.simpleParts(())(part)(Rewrite.noUpdate)

  def encode: Pipe[IO, Element, ByteVector] =
    Rewrite.simple(())(part)(Rewrite.noUpdate)
}
