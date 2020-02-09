package fs2
package pdf

import scodec.Attempt

object AnalyzeStream
{
  object SupportedCodec
  {
    def isFilter(filter: String)(data: Prim.Dict): Boolean =
      Prim.containsName("Filter")(filter)(data)

    def unapply(data: Prim.Dict): Option[Image.Codec] =
      if (isFilter("DCTDecode")(data)) Some(Image.Codec.Jpg)
      else
        if (isFilter("CCITTFaxDecode")(data)) Some(Image.Codec.Ccitt)
        else None
  }

  def apply(stream: Parsed.Stream): Obj => Attempt[Analyzed] = {
    case obj @ Obj.subtype("Image", SupportedCodec(codec)) =>
      Attempt.successful(Analyzed.Image(Image(obj, codec, stream)))
    case Obj.tpe("XRef", data) =>
      XrefStream(data)(stream).map(Analyzed.XrefStream(_))
    case obj =>
      Attempt.successful(Analyzed.Keep(obj, Some(stream.original)))
  }
}
