package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt
import scodec.bits.BitVector

case class Image1(codec: Image.Codec)

sealed trait Element

object Element
{
  sealed trait DataKind

  object DataKind
  {
    case class Page(data: Prim.Dict)
    extends DataKind

    case class Pages(kids: NonEmptyList[Prim.Ref], root: Boolean)
    extends DataKind

    case object General
    extends DataKind

    case class Array(data: Prim.Array)
    extends DataKind

    case object FontResources
    extends DataKind
  }

  case class Data(obj: Obj, kind: DataKind)
  extends Element

  sealed trait ContentKind

  object ContentKind
  {
    case class Image(image: Image1)
    extends ContentKind

    case object General
    extends ContentKind
  }

  case class Content(obj: Obj, rawStream: BitVector, stream: Eval[Attempt[BitVector]], kind: ContentKind)
  extends Element

  case class Meta(trailer: Trailer, version: Version)
  extends Element
}

object AnalyzeData
{
  def kind: Prim => Attempt[Element.DataKind] = {
    case Prim.tpe("Page", data) =>
      Attempt.successful(Element.DataKind.Page(data))
    case Prim.tpe("Pages", data) =>
      Prim.Dict.path("Kids")(data) {
        case Prim.refs(kids) => Element.DataKind.Pages(kids, data.data.contains("Parent"))
      }
    case Prim.fontResources(_) =>
      Attempt.successful(Element.DataKind.FontResources)
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

  def kind: Prim => Attempt[Element.ContentKind] = {
    case Prim.subtype("Image", SupportedCodec(codec)) =>
      Attempt.successful(Element.ContentKind.Image(Image1(codec)))
    case _ =>
      Attempt.successful(Element.ContentKind.General)
  }
}

object Elements
{
  def element: Decoded => Attempt[Element] = {
    case Decoded.DataObj(obj) =>
      AnalyzeData.kind(obj.data)
        .map(Element.Data(obj, _))
    case Decoded.ContentObj(obj, rawStream, stream) =>
      AnalyzeContent.kind(obj.data)
        .map(Element.Content(obj, rawStream, stream, _))
    case Decoded.Meta(trailer, version) =>
      Attempt.successful(Element.Meta(trailer, version))
  }

  def elementOrFail(decoded: Decoded): Stream[IO, Element] =
    StreamUtil.attemptStream(s"failed to analyze object: $decoded")(element(decoded))

  def pipe: Pipe[IO, Decoded, Element] =
    _
      .flatMap(elementOrFail)
}
