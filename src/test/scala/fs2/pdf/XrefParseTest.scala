package fs2
package pdf

import cats.data.NonEmptyList
import org.specs2.mutable.Specification

class XrefParseTest
extends Specification
{
  import Prim._

  val xrefRaw: String =
    """xref
    |0 1
    |0000507809 00000 n 
    |trailer
    |<<
    |/Size 1
    |>>
    |startxref
    |1
    |%%EOF""".stripMargin

  val index: Xref.Index =
    Xref.Index.Regular("0000507809", "00000")

  val target: Xref =
    Xref(
      NonEmptyList.one(Xref.Table(0, NonEmptyList.one(Xref.Entry(index, Xref.EntryType.InUse)))),
      Trailer(1, Prim.dict("Size" -> Prim.num(1)), None),
      1,
    )

  "xref" >>
  Xref.Codec_Xref.decode(Scodec.stringBits(xrefRaw)).toEither.map(_.value).must(beRight(target))

  val trailerRaw: String =
    """trailer
    |<< /Size 3
    |/Root 15 0 R
    |/Info 16 0 R >>
    |startxref
    |12155
    |%%EOF
    |""".stripMargin

  val trailerTarget: Trailer =
    Trailer(3, Dict(Map("Size" -> Number(3), "Root" -> Ref(15, 0), "Info" -> Ref(16, 0))), Some(Ref(15, 0)))

  "trailer" >>
  Xref.Codec_Trailer.decode(Scodec.stringBits(trailerRaw)).toEither.map(_.value).must(beRight(trailerTarget))
}
