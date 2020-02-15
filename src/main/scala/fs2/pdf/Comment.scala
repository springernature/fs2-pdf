package fs2
package pdf

import codec.{Codecs, Text}
import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.ByteVector
import scodec.codecs.{optional, provide, recover}

case class Comment(data: ByteVector)

object Comment
{
  implicit def Codec_Comment: Codec[Comment] =
    (Text.str("%").withContext("percent") ~> Text.line("content")).withContext("comment").as

  val startDecoder: Decoder[Unit] =
    Decoder { bits =>
      val bytes = bits.bytes
      if (bytes.lift(0).contains('%') && !bytes.lift(1).contains('%'))
        Attempt.successful(DecodeResult((), bytes.drop(1).bits))
      else
        Scodec.fail("not a comment")
    }

  val start: Codec[Unit] =
    Codec(provide(()), startDecoder)

  val line: Codec[ByteVector] =
    Text.line("comment")

  val inline: Codec[Option[ByteVector]] =
    optional(recover(start), line)

  val many: Codec[List[ByteVector]] =
    Codecs.many(start, line)
}
