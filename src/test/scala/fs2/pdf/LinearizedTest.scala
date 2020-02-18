package fs2
package pdf

import cats.effect.IO
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
      "Parent" -> Prim.refNum(3),
    ))

  def catalog: IndirectObj =
    IndirectObj.nostream(2, Prim.dict("Type" -> Prim.Name("Catalog"), "Pages" -> Prim.Ref(2, 0)))

  def pages: Prim.Dict =
    Prim.dict(
      "Type" -> Prim.Name("Pages"),
      "Count" -> Prim.num(2),
      "Kids" -> Prim.Array.refs(4, 1),
    )

  def content: IndirectObj =
    IndirectObj.stream(5, Prim.dict(), BitVector.empty)

  val objects: Stream[IO, IndirectObj] =
    Stream(
      page(1, 5),
      catalog,
      IndirectObj.nostream(3, pages),
      page(4, 5),
      content,
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

  "parse pdf" >>
  Test.unitStream {
    WriteLinearized.fresh(objects)
      .through(Write.bytes("test-out.pdf"))
  }
}
