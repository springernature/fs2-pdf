package fs2
package pdf

import cats.effect.IO
import fs2.Stream
import org.specs2.mutable.Specification
import scodec.bits.BitVector

class LinearizedTest
extends Specification
{
  def page(num: Long, content: Long): IndirectObj =
    IndirectObj.nostream(num, Prim.dict(
      "Type" -> Prim.Name("Page"),
      "Contents" -> Prim.refNum(content),
      "MediaBox" -> Prim.Array.nums(0, 0, 600, 800),
      "Parent" -> Prim.refNum(1),
    ))

  def pages: Prim.Dict =
    Prim.dict(
      "Type" -> Prim.Name("Pages"),
      "Count" -> Prim.num(2),
      "Kids" -> Prim.Array.refs(4, 1),
    )

  def content: IndirectObj =
    IndirectObj.stream(5, Prim.dict(), BitVector.empty)

  val parts: Stream[IO, Part[Trailer]] =
    Stream(
      Part.Version(Version.default),
      Part.Obj(IndirectObj.nostream(3, pages)),
      Part.Obj(page(4, 3)),
      Part.Obj(content),
      Part.Obj(page(1, 5)),
    )

  val linearization: Prim.Dict =
    Prim.dict(
      "Linearized" -> Prim.Number(1),
      "L" -> Prim.Number(500),
      "O" -> Prim.Number(4),
      "E" -> Prim.Number(100),
      "N" -> Prim.Number(2),
      "T" -> Prim.Number(1000),
      "H" -> Prim.Array.nums(1, 1),
    )

  val trailer: Prim.Dict =
    Prim.dict(
      "Root" -> Prim.refNum(1),
    )

  // "parse pdf" >>
  // Test.unitStream {
  //   WriteLinearized.pipe(trailer, 3, 5, 611L)(parts)
  //     .through(Write.bytes("test-out.pdf"))
  // }
}
