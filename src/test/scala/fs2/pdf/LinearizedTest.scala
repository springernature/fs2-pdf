package fs2
package pdf

import cats.effect.IO
import org.specs2.mutable.Specification

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
    IndirectObj.nostream(2, Prim.dict("Type" -> Prim.Name("Catalog"), "Pages" -> Prim.Ref(3, 0)))

  def pages: IndirectObj =
    IndirectObj.nostream(3, Prim.dict(
      "Type" -> Prim.Name("Pages"),
      "Count" -> Prim.num(2),
      "Kids" -> Prim.Array.refs(4, 1),
    ))

  def contentStream: String =
    s"""q
    |BT
    |0.5 G
    |0.5 g
    |/Helvetica 30 Tf
    |1 0 0 1 300 400 Tm
    |(HELLO) Tj
    |ET
    |Q""".stripMargin

  def content: IndirectObj =
    IndirectObj.stream(5, Prim.dict(), Scodec.stringBits(contentStream))

  val objects: Stream[IO, IndirectObj] =
    Stream(
      page(1, 5),
      catalog,
      pages,
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
