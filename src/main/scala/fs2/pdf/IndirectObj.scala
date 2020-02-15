package fs2
package pdf

import cats.implicits._
import codec.{Codecs, Newline, Whitespace, Text}
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
  import Newline.{stripNewline, lf, crlf}
  import Whitespace.{nlWs, skipWs, ws}
  import Codecs.{productCodec}
  import Text.{str}

  /**
    * Decodes the leading `stream` keyword for a content stream.
    * The standard requires the newline after this keyword to be either `\r\n` or `\n`.
    *
    * @return stream start keyword codec
    */
  def streamStartKeyword: Codec[Unit] =
    str("stream") <~ choice(lf, crlf)

  val endstreamTest: Codec[Unit] =
    ws ~> constant(Content.endstream)

  /**
    * Find the end of a content stream.
    * In the case of valid data, this amounts to simply using the 'Length' hint from the object dictionary.
    * We compensate for errors in this parameter by verifying the presence of the 'endstream' keyword right after the
    * data obtained by using the 'Length' hint and stripping manually in the error case.
    *
    * @param data The object dictionary describing the stream
    * @param bytes the remaining bytes of the object
    * @return The stream payload
    */
  def stripStream(data: Prim)(bytes: ByteVector): Attempt[DecodeResult[BitVector]] =
    for {
      end <- Content.endstreamIndex(bytes)
      length <- Content.streamLength(data)
    } yield {
      val payloadByLength = bytes.take(length).bits
      val remainderByLength = bytes.drop(length).bits
      endstreamTest.decode(remainderByLength) match {
        case Attempt.Successful(_) =>
          DecodeResult(payloadByLength, remainderByLength)
        case Attempt.Failure(_) =>
          val payload = stripNewline(bytes.take(end))
          DecodeResult(payload.bits, bytes.drop(payload.size).bits)
      }
    }

  def streamPayload(data: Prim): Codec[BitVector] =
    Codec(bits, Decoder(bits => stripStream(data)(bits.bytes)))

  def streamCodec(data: Prim): Codec[Option[BitVector]] =
    optional(recover(streamStartKeyword),
      streamPayload(data) <~ nlWs <~ str("endstream") <~ nlWs
    )

  def objHeader: Codec[Obj.Index] =
    skipWs ~> Obj.Index.Codec_Index <~ nlWs

  def prim: Codec[Prim] =
    Prim.Codec_Prim <~ nlWs

  def endobj: Codec[Unit] =
    str("endobj") <~ nlWs

  implicit def Codec_IndirectObj: Codec[IndirectObj] =
    productCodec(
      (objHeader ~ prim).flatZip { case (_, data) => streamCodec(data) } <~ endobj
    )
}

case class EncodedObj(xref: XrefObjMeta, bytes: ByteVector)

object EncodedObj
{
  def indirect(obj: IndirectObj): Attempt[EncodedObj] =
    Codecs.encodeBytes(obj).map(bytes => EncodedObj(XrefObjMeta(obj.index, bytes.size), bytes))
}
