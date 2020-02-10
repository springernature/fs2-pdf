package fs2
package pdf

import scodec.Codec
import scodec.bits.ByteVector

case class Comment(data: ByteVector)

object Comment
{
  implicit def Codec_Comment: Codec[Comment] =
    (Codecs.str("%").withContext("percent") ~> Codecs.line("content")).withContext("comment").as
}
