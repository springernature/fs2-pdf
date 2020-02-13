package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt
import scodec.bits.BitVector

case class MediaBox(x: BigDecimal, y: BigDecimal, w: BigDecimal, h: BigDecimal)

case class Page(index: Obj.Index, data: Prim.Dict, mediaBox: MediaBox)

case class Pages(index: Obj.Index, data: Prim.Dict, kids: NonEmptyList[Prim.Ref], root: Boolean)

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

  case class Meta(trailer: Trailer, version: Version)
  extends Element
}

object AnalyzeData
{
  def kind(index: Obj.Index): Prim => Attempt[Element.DataKind] = {
    case Prim.tpe("Page", data) =>
      Prim.Dict.path("MediaBox")(data) {
        case Prim.Array(List(Prim.Number(x), Prim.Number(y), Prim.Number(w), Prim.Number(h))) =>
          Element.DataKind.Page(Page(index, data, MediaBox(x, y, w, h)))
      }
    case Prim.tpe("Pages", data) =>
      Prim.Dict.path("Kids")(data) {
        case Prim.refs(kids) => Element.DataKind.Pages(Pages(index, data, kids, !data.data.contains("Parent")))
      }
    case Prim.fontResources(data) =>
      Attempt.successful(Element.DataKind.FontResource(FontResource(index, data)))
    case data @ Prim.Array(_) =>
      Attempt.successful(Element.DataKind.Array(data))
    case _ =>
      Attempt.successful(Element.DataKind.General)
  }
}

object AnalyzeContent
{
  object SupportedCodec
  {
    def unapply(data: Prim.Dict): Option[Image.Codec] =
      data("Filter") match {
        case Some(Prim.Name("DCTDecode")) => Some(Image.Codec.Jpg)
        case Some(Prim.Name("CCITTFaxDecode")) => Some(Image.Codec.Ccitt)
        case _ => None
      }
  }

  def kind(stream: Uncompressed): Prim => Attempt[Element.ContentKind] = {
    case Prim.subtype("Image", data @ SupportedCodec(codec)) =>
      Attempt.successful(Element.ContentKind.Image(Image(data, stream, codec)))
    case _ =>
      Attempt.successful(Element.ContentKind.General)
  }
}

object Elements
{
  def element: Decoded => Attempt[Element] = {
    case Decoded.DataObj(obj) =>
      AnalyzeData.kind(obj.index)(obj.data)
        .map(Element.Data(obj, _))
    case Decoded.ContentObj(obj, rawStream, stream) =>
      AnalyzeContent.kind(stream)(obj.data)
        .map(Element.Content(obj, rawStream, stream, _))
    case Decoded.Meta(_, trailer, version) =>
      Attempt.successful(Element.Meta(trailer, version))
  }

  def elementOrFail(decoded: Decoded): Stream[IO, Element] =
    StreamUtil.attemptStream(s"failed to analyze object: $decoded")(element(decoded))

  def pipe: Pipe[IO, Decoded, Element] =
    _
      .flatMap(elementOrFail)
}
