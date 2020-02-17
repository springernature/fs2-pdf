package fs2
package pdf

import codec.{Many, Text}
import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.ByteVector
import scodec.codecs.{optional, provide, recover}

private[pdf]
object Comment
{
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
    Many(start, line)
}
