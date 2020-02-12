package fs2
package pdf

import cats.implicits._
import scodec.{Attempt, Codec, DecodeResult, Decoder}
import scodec.bits.{BitVector, ByteVector}

case class IndirectObj(index: Obj.Index, data: Prim, stream: Option[BitVector])

object IndirectObj
extends IndirectObjCodec
{
  def nostream(number: Long, data: Prim): IndirectObj =
    IndirectObj(Obj.Index(number, 0), data, None)

  def addLength(stream: BitVector): Prim => Prim = {
    case Prim.Dict(data) => Prim.Dict(data.updated("Length", Prim.Number(stream.bytes.size)))
    case a => a
  }

  def ensureLength(stream: BitVector)(data: Prim): Prim =
    Prim.tryDict("Length")(data).as(data).getOrElse(addLength(stream)(data))

  def stream(number: Long, data: Prim, stream: BitVector): IndirectObj =
    IndirectObj(Obj.Index(number, 0), ensureLength(stream)(data), Some(stream))
}

private[pdf]
trait IndirectObjCodec
{
  import scodec.codecs._

  /**
    * Decodes the leading `stream` keyword for a content stream.
    * The standard requires the newline after this keyword to be either `\r\n` or `\n`.
    *
    * @return stream start keyword codec
    */
  def streamStartKeyword: Codec[Unit] =
    Codecs.str("stream") <~ choice(Codecs.lf, Codecs.crlf)

  def streamBitLength(data: Prim): Attempt[Long] =
    ParseObjects.streamLength(data).map(_ * 8)

  /**
    * Find the end of a content stream.
    * In the case of valid data, this amounts to simply using the 'Length' hint from the object dictionary.
    * We compensate for errors in this parameter by comparing the index of the `endstream` keyword with the 'Length'
    * hint and stripping manually in the error case.
    *
    * @param data The object dictionary describing the stream
    * @param bits the remaining bits of the object
    * @return The stream payload
    */
  def stripStream(data: Prim)(bits: BitVector): Attempt[DecodeResult[BitVector]] =
    for {
      end <- ParseObjects.endstreamIndex(bits)
      length <- streamBitLength(data)
      } yield {
        val payload =
          if (length > end + 16) Codecs.stripNewline(bits.take(end).bytes).bits
          else bits.take(length)
        DecodeResult(payload, bits.drop(payload.size))
      }

  def streamPayload(data: Prim): Codec[BitVector] =
    Codec(bits, Decoder(stripStream(data) _))

  def streamCodec(data: Prim): Codec[Option[BitVector]] =
    optional(recover(streamStartKeyword),
      streamPayload(data) <~ Codecs.newline <~ Codecs.str("endstream") <~ Codecs.nlWs
    )

  def objHeader: Codec[Obj.Index] =
    Codecs.skipWs ~> Obj.Index.Codec_Index <~ Codecs.nlWs

  def prim: Codec[Prim] =
    Prim.Codec_Prim <~ Codecs.nlWs

  def endobj: Codec[Unit] =
    Codecs.str("endobj") <~ Codecs.nlWs

  implicit def Codec_IndirectObj: Codec[IndirectObj] =
    Codecs.productCodec(
      (objHeader ~ prim).flatZip { case (_, data) => streamCodec(data) } <~ endobj
    )
}

case class EncodedObj(xref: XrefObjMeta, bytes: ByteVector)

object EncodedObj
{
  def indirect(obj: IndirectObj): Attempt[EncodedObj] =
    Codecs.encodeBytes(obj).map(bytes => EncodedObj(XrefObjMeta(obj.index, bytes.size), bytes))
}
