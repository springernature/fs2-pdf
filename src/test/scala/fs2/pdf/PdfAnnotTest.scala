package fs2
package pdf

import cats.effect.IO
import fs2.Stream
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class PdfAnnotTest
extends Specification
{
  import BasicResources.cs
  import Obj.Index

  val page1ObjNumber: Int = 1

  val page2ObjNumber: Int = 2

  def page1Obj: IndirectObj =
    IndirectObj.nostream(page1ObjNumber, 0, Prim.dict(
      "Type" -> Prim.Name("Page"),
      "Contents" -> Prim.refT(contentObj),
      "MediaBox" -> Prim.array(Prim.num(0), Prim.num(0), Prim.num(600), Prim.num(800)),
      "Parent" -> Prim.refT(pagesObj),
      "Resources" -> Prim.refT(resourcesObj),
      "Annots" -> Prim.array(Prim.refT(annotObj)),
    ))

  def page2Obj: IndirectObj =
    IndirectObj.nostream(page2ObjNumber, 0, Prim.dict(
      "Type" -> Prim.Name("Page"),
      "Contents" -> Prim.refT(contentObj),
      "MediaBox" -> Prim.array(Prim.num(0), Prim.num(0), Prim.num(600), Prim.num(800)),
      "Parent" -> Prim.refT(pagesObj),
      "Resources" -> Prim.refT(resourcesObj),
      "Annots" -> Prim.array(Prim.refT(annotObj)),
    ))

  def pages: List[Prim] =
    List(Prim.Ref(page1ObjNumber, 0), Prim.Ref(page2ObjNumber, 0))

  def pagesObj: IndirectObj =
    IndirectObj.nostream(3, 0, Prim.dict(
      "Type" -> Prim.Name("Pages"),
      "Count" -> Prim.num(pages.length),
      "Kids" -> Prim.Array(pages),
    ))

  def catalogObj: IndirectObj =
    IndirectObj.nostream(4, 0, Prim.dict(
      "Type" -> Prim.Name("Catalog"),
      "Pages" -> Prim.refT(pagesObj),
      "Outlines" -> Prim.refT(outlinesObj),
  ))

  def contentStream: ByteVector =
    ByteVector(
      """BT
      |  /F1 24 Tf
      |  50 750 Td
      |  (Hello World) Tj
      |ET""".stripMargin.getBytes
    )

  def contentObj: IndirectObj =
    IndirectObj(Index(5, 0), Prim.dict("Length" -> Prim.num(contentStream.size)),  Some(contentStream.bits))

  def resourcesObj: IndirectObj =
    IndirectObj.nostream(6, 0, Prim.dict(
      "ProcSet" -> Prim.array(Prim.Name("PDF"), Prim.Name("Text")),
      "Font" -> Prim.dict("F1" -> Prim.refT(fontObj)),
    ))

  def fontObj: IndirectObj =
    IndirectObj.nostream(7, 0, Prim.dict(
      "Type" -> Prim.Name("Font"),
      "Subtype" -> Prim.Name("Type1"),
      "Name" -> Prim.Name("F1"),
      "BaseFont" -> Prim.Name("Helvetica"),
      "Encoding" -> Prim.Name("MacRomanEncoding"),
    ))

  def outlinesObj: IndirectObj =
    IndirectObj.nostream(8, 0, Prim.dict(
      "Type" -> Prim.Name("Outlines"),
      "Count" -> Prim.num(0),
    ))

  def infoObj: IndirectObj =
    IndirectObj.nostream(9, 0, Prim.dict())

  def annotObj: IndirectObj =
    IndirectObj.nostream(10, 0, Prim.dict(
      "Type" -> Prim.Name("Annot"),
      "Subtype" -> Prim.Name("FreeText"),
      "Rect" -> Prim.array(Prim.num(200), Prim.num(50), Prim.num(400), Prim.num(150)),
      "Contents" -> Prim.str("the annotation"),
      "Border" -> Prim.array(Prim.num(0), Prim.num(0), Prim.num(0)),
    ))

  def objects: List[IndirectObj] =
    List(
      page1Obj,
      page2Obj,
      pagesObj,
      catalogObj,
      contentObj,
      resourcesObj,
      fontObj,
      outlinesObj,
      infoObj,
      annotObj,
    )

  def id: Prim =
    Prim.HexStr(ByteVector("FF1FE073E0365E226E145B0CC9CB0758".getBytes))

  def trailer: Prim.Dict =
    Prim.dict(
      "Size" -> Prim.num(objects.size + 1),
      "Root" -> Prim.refT(catalogObj),
      "Id" -> Prim.array(id, id),
      "Info" -> Prim.refT(infoObj),
    )

  def write(outfile: String): Stream[IO, Unit] =
    Stream.emits(objects)
      .through(Generate(trailer))
      .through(WriteFile(outfile))

  // "write to file" >>
  // write("../generated.pdf").compile.drain.unsafeRunSync.must_==(())

  def encode: IO[String] =
    Stream
      .emits(objects)
      .through(Generate(trailer))
      .compile
      .to(Array)
      .map(new String(_))

  def test: IO[MatchResult[Any]] =
    for {
      target <- Jar.resource("annot-test-target.pdf").value
      result <- encode
    } yield Right(result).must_==(target)

  "encode a pdf file with free text annotation from data" >>
  test.unsafeRunSync
}
