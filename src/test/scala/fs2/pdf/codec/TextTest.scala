package fs2
package pdf
package codec

import org.specs2.mutable.Specification
import scodec.DecodeResult
import scodec.bits.BitVector

class TextTest
extends Specification
{
  val line: String =
    "line content"

  val bits: BitVector =
    Scodec.stringBits(s"$line\n")

  "decode a line" >>
  Text.line("test")
    .decode(bits)
    .toEither
    .must(beRight(DecodeResult(Scodec.stringBytes(line), BitVector.empty)))
}
