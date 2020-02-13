package fs2
package pdf

import cats.Eval
import scodec.{Attempt, Err}
import scodec.bits.{BitVector, ByteVector}

case class Uncompressed(stream: Eval[Attempt[BitVector]])
{
  def exec: Attempt[BitVector] =
    stream.value
}

object Content
{
  def extractObjectStream(stream: Uncompressed): Prim => Option[Attempt[ObjectStream]] = {
    case Prim.tpe("ObjStm", _) =>
      Some(
        stream
          .stream
          .value
          .flatMap(ObjectStream.Codec_ObjectStream.complete.decode)
          .map(_.value)
      )
    case _ =>
      None
  }

  def uncompress(stream: BitVector)
  : Prim => Uncompressed = {
    case Prim.filter("FlateDecode", data) =>
      Uncompressed(Eval.later(FlateDecode(stream, data)))
    case _ =>
      Uncompressed(Eval.now(Attempt.successful(stream)))
  }

  def streamLength(dict: Prim): Attempt[Long] =
    Prim.Dict.number("Length")(dict).map(_.toLong)

  val streamEndMarker: ByteVector =
    ByteVector("endstream".getBytes)

  def endstreamIndex(bytes: ByteVector): Attempt[Long] =
    bytes.indexOfSlice(streamEndMarker) match {
      case i if i >= 0 => Attempt.successful(i)
      case _ => Attempt.failure(Err.InsufficientBits(0, bytes.bits.size, List("no stream end position found")))
    }
}
