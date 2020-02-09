package fs2
package pdf

import scodec.Codec
import scodec.bits.ByteVector

case class Version(major: Int, minor: Int, binaryMarker: Option[ByteVector])

object Version
{
  implicit def Codec_Version: Codec[Version] =
    Codecs.productCodec(
      Codecs.str("%PDF-") ~>
      (Codecs.ascii.int ~ (Codecs.constantString(".") ~> Codecs.ascii.int) <~ Codecs.nlWs) ~
      (Codecs.str("%") ~> Codecs.opt(scodec.codecs.bytes) <~ Codecs.nlWs)
        // Codecs.opt(Codecs.line("binary marker"))
    )

  def default: Version =
    Version(1, 7, Some(ByteVector("âãÏÓ".getBytes)))
}
