package fs2
package pdf

import cats.implicits._
import scodec.{Attempt, Codec}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.bits

case class IndirectObj(index: Obj.Index, data: Prim, stream: Option[BitVector])

object IndirectObj
{
  def streamCodec: Codec[BitVector] =
    Codecs.constantLine("stream") ~> bits <~ Codecs.newline <~ Codecs.constantLine("endstream")

  implicit def Codec_IndirectObj: Codec[IndirectObj] =
    Codecs.productCodec(
      (Obj.Index.Codec_Index <~ Codecs.linuxNewline) ~
      (Prim.Codec_Prim <~ Codecs.linuxNewline) ~
      Codecs.opt(streamCodec) <~
      Codecs.constantLine("endobj")
    )

  def nostream(number: Long, generation: Int, data: Prim): IndirectObj =
    IndirectObj(Obj.Index(number, generation), data, None)

  def addLength(stream: BitVector): Prim => Prim = {
    case Prim.Dict(data) => Prim.Dict(data.updated("Length", Prim.Number(stream.bytes.size)))
    case a => a
  }

  def ensureLength(stream: BitVector)(data: Prim): Prim =
    Prim.tryDict("Length")(data).as(data).getOrElse(addLength(stream)(data))

  def stream(number: Long, generation: Int, data: Prim, stream: BitVector): IndirectObj =
    IndirectObj(Obj.Index(number, generation), ensureLength(stream)(data), Some(stream))
}

case class EncodedObj(xref: XrefObjMeta, bytes: ByteVector)

object EncodedObj
{
  def indirect(obj: IndirectObj): Attempt[EncodedObj] =
    Codecs.encodeBytes(obj).map(bytes => EncodedObj(XrefObjMeta(obj.index, bytes.size), bytes))
}
