package fs2
package pdf

import cats.implicits._
import org.specs2.matcher.MatchResult
import org.specs2.mutable.Specification
import scodec.{Codec, Decoder}
import scodec.bits.{BitVector, ByteVector}

class PrimTest
extends Specification
{
  import Prim._

  def decode[A: Decoder](in: BitVector): Either[String, A] =
    Decoder[A].decode(in).toEither.bimap(_.toString, _.value)

  def testDecode[A: Decoder](raw: String, data: A): MatchResult[Any] =
    decode[A](BitVector(raw.getBytes))
      .must(beRight(data))

  def testAs[A](codec: Codec[A])(raw: String, data: A, encoded: String): MatchResult[Any] =
    codec
      .encode(data)
      .toEither
      .flatMap(enc => enc.decodeUtf8.leftMap(_.toString).product(decode(BitVector(raw.getBytes))(codec)))
      .must(beRight((encoded, data)))

  def testWith[A](codec: Codec[A])(raw: String, data: A): MatchResult[Any] =
    codec
      .encode(data)
      .toEither
      .flatMap(enc => enc.decodeUtf8.leftMap(_.toString).product(decode(enc)(codec)))
      .must(beRight((raw, data)))

  def test(data: String, result: Prim): MatchResult[Any] =
    testWith(Prim.Codec_Prim)(data, result)

  val ref: String =
    "11 0 R"

  "ref" >>
  testWith(Prim.Codec_Ref)(ref, Prim.Ref(11, 0))

  val array: String =
    "[1 0 R 2 3 4 R ]"

  "array" >>
  test(array, Array(List(Ref(1, 0), Prim.num(2), Ref(3, 4))))

  val name: String =
    "/name"

  "name" >>
  test(name, Name("name"))

  val float: String =
    "555.333"

  "float" >>
  test(float, Number(555.333))

  val negative: String =
    "-512"

  "negative" >>
  test(negative, Number(-512))

  val hexstr: String =
    "<0FD96C035A0F15D3FEB3755C6D76FC35>"

  "hexstr" >>
  testWith(Prim.Codec_HexStr)(hexstr, hexStr("0FD96C035A0F15D3FEB3755C6D76FC35"))

  val dict: String =
    "<</foo/bar/zoo[ 1 2 R]/moo<</sub/dict/bool false/null null>>>>"

  val dictTarget: Prim.Dict =
    Prim.dict(
      "foo" -> Name("bar"),
      "zoo" -> Array(List(Ref(1, 2))),
      "moo" -> Prim.dict(
        "sub" -> Name("dict"),
        "bool" -> Bool(false),
        "null" -> Null,
      ),
    )

  val dictTargetEncoded: String =
    """<<
    |/foo /bar
    |/zoo [1 2 R ]
    |/moo <<
    |/sub /dict
    |/bool false
    |/null null
    |>>
    |>>""".stripMargin

  "dict" >>
  testAs(Prim.Codec_Dict)(dict, dictTarget, dictTargetEncoded)

  val dict2: String =
    """5394 0 obj
    |<< /Type /ObjStm /Filter /FlateDecode /First 1019 /Length 4180 /N 100
    |>>
    |""".stripMargin

  val dict2Target: Prim.Dict =
    Prim.dict(
      "Type" -> Name("ObjStm"),
      "Filter" -> Name("FlateDecode"),
      "First" -> Number(1019),
      "Length" -> Number(4180),
      "N" -> Number(100),
    )

  "dict 2" >>
  testDecode(dict2, Obj(Obj.Index(5394, 0), dict2Target))(Obj.codecPreStream)

  val escaped: String =
    "hello \\) parens"

  "string with escaped parens" >>
  testWith(Prim.Codec_Str)(s"($escaped)", Str(ByteVector(escaped.getBytes)))

  val nested: String =
    "with (nested (parens)) here"

  "nested" >>
  test(s"($nested)", Str(ByteVector(nested.getBytes)))

  val obj: String =
    s"""10 0 obj
    |<<
    |/Type /Page
    |/MediaBox [0 0 595.276 841.89 ]
    |/Parent 5 0 R
    |/Nested (foo (nest) bar)
    |>>
    |endobj
    |""".stripMargin

  def objTarget: Prim.Dict =
    Prim.dict(
      "Type" -> Name("Page"),
      "MediaBox" -> Array(List(Prim.num(0), Prim.num(0), Prim.num(595.276), Prim.num(841.89))),
      "Parent" -> Ref(5, 0),
      "Nested" -> Prim.str("foo (nest) bar"),
    )

  "obj" >>
  testWith(Obj.Codec_Obj)(obj, Obj(Obj.Index(10, 0), objTarget))

  val emptyArray: String =
    """<< /Fields
    |[
    |]
    |>>
    |""".stripMargin

  "empty array" >>
  testDecode(emptyArray, Prim.dict("Fields" -> Array(Nil)))(Prim.Codec_Dict)

  val nul: String =
    """
    |13122 0 obj
    |<<
    |/D [1512 0 R /XYZ 208 207 null ]
    |>>
    |endobj
    |""".stripMargin

  val nullTarget: Prim =
    Prim.dict("D" -> Prim.array(Ref(1512, 0), Name("XYZ"), Number(208), Number(207), Null))

  "null" >>
  testDecode(nul, Obj(Obj.Index(13122, 0), nullTarget))(Obj.Codec_Obj)
}
