package fs2
package pdf

import codec.{Many, Text, Whitespace}
import scodec.Codec

/**
  * An object stream is an indirect object with the '/Type' field set to '/ObjStm'.
  *
  * It contains multiple compressed data objects (without streams).
  *
  * @param objs the data objects contained in the stream
  */
case class ObjectStream(objs: List[Obj])

object ObjectStream
extends ObjectStreamCodec

private[pdf]
trait ObjectStreamCodec
{
  import scodec.codecs.{listOfN, provide}

  def encode(os: ObjectStream): (List[Long], List[Prim]) =
    os.objs.map(a => (a.index.number, a.data)).unzip

  def decode: ((List[Long], List[Prim])) => ObjectStream = {
    case (numbers, objects) =>
      ObjectStream(
        numbers
          .zip(objects)
          .map { case (num, data) => Obj(Obj.Index(num, 0), data) }
      )
  }

  // TODO this sets the offset to 0. it should be calculated from the length of the encoded objects
  // so this must probably be completely separate de- and encoders
  def number: Codec[Long] =
    Text.ascii.long <~ Whitespace.ws <~ Text.ascii.long.unit(0) <~ Whitespace.ws

  implicit def Codec_ObjectStream: Codec[ObjectStream] =
    Many.till(a => !a.bytes.headOption.exists(Text.isDigit))(number)
      .flatZip(numbers => listOfN(provide(numbers.size), Prim.Codec_Prim))
      .xmap(decode, encode _)
}
