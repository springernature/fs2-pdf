package fs2
package pdf

import cats.implicits._
import codec.{Codecs, Newline, Text, Whitespace}
import scodec._
import scodec.bits._

/**
  * An indirect object is the main syntactic element at the top level of a PDF file.
  *
  * Their shape is:
  * {{{
  * 5 0 object
  * <<
  * /Key1 /Value1
  * /Key2 [1 2 3]
  * stream
  * <rendering instructions, image data or other binary data>
  * endstream
  * endobj
  * }}}
  *
  * where the 'stream' part is optional
  *
  * @param obj the index and data part
  * @param stream the optional content stream part
  */
case class IndirectObj(obj: Obj, stream: Option[BitVector])

object IndirectObj
extends IndirectObjCodec
{
  def nostream(number: Long, data: Prim): IndirectObj =
    IndirectObj(Obj(Obj.Index(number, 0), data), None)

  private[pdf]
  def addLength(stream: BitVector): Prim => Prim = {
    case Prim.Dict(data) => Prim.Dict(data.updated("Length", Prim.Number(stream.bytes.size)))
    case a => a
  }

  private[pdf]
  def ensureLength(stream: BitVector)(data: Prim): Prim =
    Prim.tryDict("Length")(data).as(data).getOrElse(addLength(stream)(data))

  def stream(number: Long, data: Prim, stream: BitVector): IndirectObj =
    IndirectObj(Obj(Obj.Index(number, 0), ensureLength(stream)(data)), Some(stream))

  object number
  {
    /**
      * pattern-match the object number of an [[IndirectObj]]
      *
      * @param obj
      * @return the object number
      */
    def unapply(obj: IndirectObj): Option[Long] =
      Some(obj.obj.index.number)
  }

  object dict
  {
    /**
      * pattern-match if the [[Prim]] in the data part is a [[Prim.Dict]]
      *
      * @param obj
      * @return the object number and a [[Prim.Dict]]
      */
    def unapply(obj: IndirectObj): Option[(Long, Prim.Dict)] =
      obj match {
        case IndirectObj(Obj.dict(number, dict), _) =>
          Some(number, dict)
        case _ =>
          None
      }
  }
}

private[pdf]
trait IndirectObjCodec
{
  import scodec.codecs._
  import Newline.{stripNewline, lf, crlf}
  import Whitespace.{nlWs, skipWs, ws}
  import Text.str

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

  val objHeader: Codec[Obj.Index] =
    skipWs ~> Obj.Index.Codec_Index <~ nlWs

  val prim: Codec[Prim] =
    Prim.Codec_Prim <~ nlWs

  val endobj: Codec[Unit] =
    str("endobj") <~ nlWs

  val preStream: Codec[Obj] =
    (objHeader :: prim)
      .as[Obj]

  val stream: Obj => Codec[Option[BitVector]] = {
    case Obj(_, data) =>
      streamCodec(data)
  }

  implicit def Codec_IndirectObj: Codec[IndirectObj] =
    (preStream.flatZip(stream) <~ endobj)
      .flattenLeftPairs
      .as[IndirectObj]
}

/**
  * Helper type for encoding the object and its xref entry
  *
  * @param xref the metadata relevant for the xref entry
  * @param bytes the byte-encoded [[IndirectObj]]
  */
case class EncodedObj(xref: XrefObjMeta, bytes: ByteVector)

object EncodedObj
{
  /**
    * Encode an [[IndirectObj]] and package it with the xref metadata
    *
    * @param obj
    * @return an encoded object with its metadata
    */
  def indirect(obj: IndirectObj): Attempt[EncodedObj] =
    Codecs.encodeBytes(obj)
      .map(bytes => EncodedObj(XrefObjMeta(obj.obj.index, bytes.size), bytes))
}
