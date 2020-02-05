package fs2
package pdf

import cats.effect.IO
import fs2.Stream
import org.specs2.mutable.Specification
import scodec.bits.{BitVector, HexStringSyntax}
import scodec.interop.cats.BitVectorMonoidInstance

class GenerateTiffPdfTest
extends Specification
{
  import Obj.Index
  import Prim._

  def page1Obj(image: IndirectObj): IndirectObj =
    IndirectObj.nostream(1, 0, Prim.dict(
      "Type" -> Prim.Name("Page"),
      "Contents" -> Prim.refT(image),
      "MediaBox" -> Prim.array(Prim.num(0), Prim.num(0), Prim.num(600), Prim.num(800)),
      "Parent" -> Prim.refT(pagesObj),
    ))

  def pages: List[Prim] =
    List(Prim.Ref(1, 0))

  def pagesObj: IndirectObj =
    IndirectObj.nostream(2, 0, Prim.dict(
      "Type" -> Prim.Name("Pages"),
      "Count" -> Prim.num(pages.length),
      "Kids" -> Prim.Array(pages),
    ))

  def catalogObj: IndirectObj =
    IndirectObj.nostream(3, 0, Prim.dict(
      "Type" -> Prim.Name("Catalog"),
      "Pages" -> Prim.refT(pagesObj),
  ))

  def imageDict: Prim.Dict =
    Prim.dict(
      "Length" -> Number(985),
      "BitsPerComponent" -> Number(1),
      "Decode" -> Array(List(Number(0), Number(1))),
      "Height" -> Number(189),
      "Filter" -> Name("CCITTFaxDecode"),
      "Subtype" -> Name("Image"),
      "Intent" -> Name("RelativeColorimetric"),
      "Width" -> Number(841),
      "ImageMask" -> Bool(true),
      "Type" -> Name("XObject"),
      "DecodeParms" -> Dict(Map("Columns" -> Number(841), "K" -> Number(-1), "Rows" -> Number(189))),
    )

  def imageObj(imageStream: BitVector): IndirectObj =
    IndirectObj(Index(4, 0), imageDict, Some(imageStream))

  def id: Prim =
    Prim.HexStr(hex"FF1FE073E0365E226E145B0CC9CB0758")

  def trailer: Prim.Dict =
    Prim.dict(
      "Size" -> Prim.num(5),
      "Root" -> Prim.refT(catalogObj),
      "Id" -> Prim.array(id, id),
    )

  def objs(image: BitVector): List[IndirectObj] =
  {
    val imageContent = imageObj(image)
    List(
      page1Obj(imageContent),
      pagesObj,
      catalogObj,
      imageContent,
    )
  }

  def objsS(image: Stream[IO, Byte]): Stream[IO, IndirectObj] =
    image.chunks.map(_.toBitVector).foldMonoid
      .flatMap { bits =>
        Stream.emits(objs(bits))
      }

  def gen: Stream[IO, Unit] =
    objsS(Jar.resourceStreamData("tiff-data"))
      .through(Generate(trailer))
      .through(WriteFile("../test-image-ccitt.pdf"))

  // "gen" >>
  // gen.compile.drain.as(1 must_== 1).unsafeRunSync
}
