package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.Attempt

private[pdf]
object AnalyzeData
{
  val infoKeys: List[String] =
    List("Author", "Producer", "Creator", "CreationData", "ModDate")

  def apply(index: Obj.Index): Prim => Attempt[Element.DataKind] = {
    case Prim.tpe("Page", data) =>
      Page.fromData(index)(data).map(Element.DataKind.Page(_))
    case Prim.tpe("Pages", data) =>
      Pages.fromData(index)(data).map(Element.DataKind.Pages(_))
    case Prim.fontResources(data) =>
      Attempt.successful(Element.DataKind.FontResource(FontResource(index, data)))
    case data @ Prim.Dict(dict) if infoKeys.exists(dict.contains) =>
      Attempt.successful(Element.DataKind.Info(data))
    case data @ Prim.Array(_) =>
      Attempt.successful(Element.DataKind.Array(data))
    case _ =>
      Attempt.successful(Element.DataKind.General)
  }
}

private[pdf]
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

  def apply(stream: Uncompressed): Prim => Attempt[Element.ContentKind] = {
    case Prim.subtype("Image", data @ SupportedCodec(codec)) =>
      Attempt.successful(Element.ContentKind.Image(Image(data, stream, codec)))
    case _ =>
      Attempt.successful(Element.ContentKind.General)
  }
}

/**
  * @see [[Element]]
  */
object Elements
{
  private[pdf]
  def element: Decoded => Attempt[Element] = {
    case Decoded.DataObj(obj) =>
      AnalyzeData(obj.index)(obj.data)
        .map(Element.Data(obj, _))
    case Decoded.ContentObj(obj, rawStream, stream) =>
      AnalyzeContent(stream)(obj.data)
        .map(Element.Content(obj, rawStream, stream, _))
    case Decoded.Meta(_, trailer, version) =>
      Attempt.successful(Element.Meta(trailer, version))
  }

  private[pdf]
  def elementOrFail(decoded: Decoded): Stream[IO, Element] =
    StreamUtil.attemptStream(s"failed to analyze object: $decoded")(element(decoded))

  /**
    * @return a [[Pipe]] that semantically analyzes objects
    */
  def pipe: Pipe[IO, Decoded, Element] =
    _
      .flatMap(elementOrFail)
}
