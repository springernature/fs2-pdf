package fs2
package pdf

import org.specs2.mutable.Specification
import scodec.DecodeResult
import scodec.bits.BitVector

class InvalidLengthTest
extends Specification
{
  val streamBytes: Array[Byte] =
    Array.fill(100)('x')

  val streamData: String =
    new String(streamBytes)

  val data: String =
    s"""1 0 obj
    |<</Length 50>>
    |stream
    |$streamData
    |endstream
    |endobj""".stripMargin

  val bits: BitVector =
    Scodec.stringBits(data)

  val dict: Prim.Dict =
    Prim.dict(
      "Length" -> Prim.Number(50)
    )

  val target: IndirectObj =
    IndirectObj.stream(1, dict, BitVector(streamBytes))

  "invalid stream length in dictionary" >>
  IndirectObj.Codec_IndirectObj
    .decode(bits)
    .toEither
    .must(beRight(DecodeResult(target, BitVector.empty)))
}
