package fs2
package pdf

import codec.{Codecs, Text, Whitespace}
import scodec.Codec
import scodec.bits.ByteVector

case class Version(major: Int, minor: Int, binaryMarker: Option[ByteVector])

object Version
{
  import Text.{str, ascii, line}

  implicit def Codec_Version: Codec[Version] =
    Codecs.productCodec(
      str("%PDF-") ~>
      (ascii.int ~ (str(".") ~> ascii.int) <~ Whitespace.nlWs) ~
      Codecs.opt(str("%") ~> line("binary marker"))
    )

  def default: Version =
    Version(1, 7, Some(ByteVector("âãÏÓ".getBytes)))
}
