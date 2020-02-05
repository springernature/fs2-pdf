package fs2
package pdf

import cats.effect.IO
import fs2.Pipe
import scodec.bits.{BitVector, ByteVector}

object Write
{
  def bits(outfile: String): Pipe[IO, BitVector, Unit] =
    in =>
      WriteFile(outfile)(in.flatMap(StreamUtil.bits))

  def bytes(outfile: String): Pipe[IO, ByteVector, Unit] =
    in =>
      WriteFile(outfile)(in.flatMap(StreamUtil.bytes))
}
