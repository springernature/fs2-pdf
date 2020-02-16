package fs2
package pdf

import org.specs2.mutable.Specification
import scodec.DecodeResult
import scodec.bits.BitVector

class CommentTest
extends Specification
{
  val data: String =
    s"""1 0 obj % comment
    |% comment
    |<<% comment
    |% comment
    |/Length 50 % comment
    |% comment
    |>> % comment
    |% comment
    |% comment
    |endobj %comment
    |""".stripMargin

  val bits: BitVector =
    BitVector(data.getBytes)

  val dict: Prim.Dict =
    Prim.dict(
      "Length" -> Prim.Number(50)
    )

  val target: IndirectObj =
    IndirectObj(Obj(Obj.Index(1, 0), dict), None)

  "invalid stream length in dictionary" >>
  IndirectObj.Codec_IndirectObj
    .decode(bits)
    .toEither
    .must(beRight(DecodeResult(target, BitVector.empty)))

  def multiComment(count: Int): BitVector =
    BitVector(List.fill(count)("% commment\n").mkString.getBytes)

  "stack safety" >>
  Comment.many.decode(multiComment(3000))
    .toEither
    .map(_.value.size)
    .must(beRight(3000))
}
