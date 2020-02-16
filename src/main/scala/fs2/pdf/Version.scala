package fs2
package pdf

import java.nio.charset.StandardCharsets

import codec.{Codecs, Text, Whitespace}
import scodec.Codec
import scodec.bits.ByteVector
import shapeless.{::, HNil}

case class Version(major: Int, minor: Int, binaryMarker: Option[ByteVector])

object Version
{
  import Text.{str, ascii, line, char}
  import Codecs.opt
  import Whitespace.nlWs

  def versionLine: Codec[Int :: Int :: HNil] =
    str("%PDF-") ~> ascii.int :: (char('.') ~> ascii.int <~ nlWs)

  def binaryMarker: Codec[Option[ByteVector]] =
    opt(char('%') ~> line("binary marker"))

  implicit def Codec_Version: Codec[Version] =
    (versionLine :+ binaryMarker)
      .as[Version]

  def default: Version =
    Version(1, 7, Some(ByteVector("âãÏÓ".getBytes(StandardCharsets.ISO_8859_1))))
}
