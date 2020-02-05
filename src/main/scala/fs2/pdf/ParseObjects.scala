package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}
import scodec.{Attempt, Decoder}
import scodec.bits.{BitVector, ByteVector}
import scodec.interop.cats._

sealed trait Parsed

object Parsed
{
  case class Stream(original: BitVector, data: Eval[Attempt[BitVector]])

  case class IndirectObj(obj: Obj, stream: Option[Parsed.Stream], original: ByteVector)
  extends Parsed

  case class StreamObject(obj: Obj)
  extends Parsed

  case class Unparsable(index: Obj.Index, data: ByteVector)
  extends Parsed

  case class Raw(bytes: ByteVector)
  extends Parsed

  def indirectObjs: Pipe[IO, Parsed, pdf.IndirectObj] =
    _.flatMap {
      case IndirectObj(obj, stream, _) =>
        stream.map(_.data.value)
          .traverse(StreamUtil.attemptStream("Parsed.indirectObjs"))
          .map(pdf.IndirectObj(obj.index, obj.data, _))
      case StreamObject(obj) =>
        fs2.Stream(pdf.IndirectObj(obj.index, obj.data, None))
      case Unparsable(_, _) =>
        fs2.Stream.empty
      case Raw(_) =>
        fs2.Stream.empty
    }
}

object ParseObjects
{
  def extractStreamObjects(stream: Eval[Attempt[BitVector]]): Obj => Attempt[Option[NonEmptyList[Parsed]]] = {
    case Obj.tpe("ObjStm", _) =>
      stream
        .value
        .flatMap(ObjectStream.Codec_ObjectStream.decode)
        .map(_.value.objs.map(Parsed.StreamObject(_)))
        .map(NonEmptyList.fromList)
    case _ =>
      Attempt.successful(None)
  }

  val streamEndMarker: ByteVector =
    ByteVector("endstream".getBytes)

  def streamLength(dict: Prim): Attempt[Long] =
    Prim.Dict.number("Length")(dict).map(_.toLong)

  def stripStreamEndMarker(bits: BitVector): Attempt[BitVector] =
    bits.bytes.indexOfSlice(streamEndMarker) match {
      case i if i >= 0 => Attempt.successful(Codecs.stripNewline(bits.bytes.take(i)).bits)
      case _ => Codecs.fail("no stream end position found")
    }

  def streamStartMarkerDecoder: Decoder[Unit] = {
    import scodec.codecs.choice
    Codecs.constantString("stream") <~ choice(Codecs.windowsNewline, Codecs.linuxNewline)
  }

  /**
   * Remove the trailing 'endstream\nendobj\n' from the stream data.
   * In the case of valid data, this amounts to simply using the 'Length' hint from the object dictionary.
   * We compensate for errors in this parameter by comparing the length of the data minus 136 bits for the keywords
   * with the 'Length' hint and stripping manually in the error case.
   */
  def stripStream(length: Long)(bits: BitVector): Attempt[BitVector] =
    for {
      withoutStartMarker <- streamStartMarkerDecoder.decode(bits).map(_.remainder)
      withoutEndMarker <-
      if (length > withoutStartMarker.size - 136) stripStreamEndMarker(withoutStartMarker)
      else Attempt.successful(withoutStartMarker.take(length))
    } yield withoutEndMarker

  def uncompressStrippedStream(stream: BitVector)
  : Prim => Eval[Attempt[BitVector]] = {
    case Prim.filter("FlateDecode", data) =>
      Eval.later(FlateDecode(stream, data))
    case _ =>
      Eval.now(Attempt.successful(stream))
  }

  def uncompressStream[A](data: Prim)(stream: BitVector)
  : Attempt[Parsed.Stream] =
    for {
      length <- streamLength(data)
      bits <- stripStream(length * 8)(stream)
    } yield Parsed.Stream(bits, uncompressStrippedStream(bits)(data))

  def parseObject(text: ByteVector): Attempt[Obj] =
    Obj.codecPreStream.decode(text.bits).map(_.value)

  def outputObjs(obj: Obj, stream: Option[Parsed.Stream], text: ByteVector, rawStream: Option[BitVector])
  : Option[NonEmptyList[Parsed]] => List[Parsed] = {
    case Some(streamObjs) =>
      streamObjs.toList
    case None =>
      List(Parsed.IndirectObj(obj, stream, text ++ rawStream.combineAll.bytes))
  }

  def processObject(text: ByteVector, rawStream: Option[BitVector]): Attempt[List[Parsed]] =
    for {
      obj <- parseObject(text)
      uncompressed <- rawStream.traverse(uncompressStream(obj.data))
      streamObjs <- uncompressed.map(_.data).flatTraverse(extractStreamObjects(_)(obj))
    } yield outputObjs(obj, uncompressed, text, rawStream)(streamObjs)

  def processChunk: ObjectChunk => Stream[IO, Parsed] = {
    case ObjectChunk.Text(bytes) =>
      Stream.emit(Parsed.Raw(bytes))
    case ObjectChunk.Object(_, text, stream) =>
      processObject(text, stream) match {
        case Attempt.Successful(parsed) =>
          Stream.emits(parsed)
        case Attempt.Failure(cause) =>
          StreamUtil.failStream(s"${cause.messageWithContext}; when parsing ${Codecs.sanitizedLatin.decode(text.bits)}")
      }
  }

  def pipe: Pipe[IO, ObjectChunk, Parsed] =
    _.flatMap(processChunk)
}
