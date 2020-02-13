package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}
import scodec.{Attempt, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}

sealed trait Parsed

object Parsed
{
  case class Stream(original: BitVector, data: Eval[Attempt[BitVector]])

  case class IndirectObj(obj: Obj, stream: Option[Parsed.Stream], original: ByteVector)
  extends Parsed

  case class StreamObject(obj: Obj)
  extends Parsed

  case class StartXref(startxref: pdf.StartXref)
  extends Parsed

  case class Xref(xref: pdf.Xref)
  extends Parsed

  case class Version(version: pdf.Version)
  extends Parsed

  def indirectObjs: Pipe[IO, Parsed, pdf.IndirectObj] =
    _.flatMap {
      case IndirectObj(obj, stream, _) =>
        stream.map(_.data.value)
          .traverse(StreamUtil.attemptStream("Parsed.indirectObjs"))
          .map(pdf.IndirectObj(obj.index, obj.data, _))
      case StreamObject(obj) =>
        fs2.Stream(pdf.IndirectObj(obj.index, obj.data, None))
      case _ =>
        fs2.Stream.empty
    }
}

object ParseNonObject
{
  def withoutComments[A](inner: Decoder[A]): Decoder[A] =
    Decoder(bits => inner.decode(Codecs.removeCommentsBits(bits)))

  def decoder: Decoder[Parsed] =
    Decoder.choiceDecoder(
      withoutComments(StartXref.Codec_StartXref).map(Parsed.StartXref(_)),
      withoutComments(Xref.Codec_Xref).map(Parsed.Xref(_)),
      Version.Codec_Version.map(Parsed.Version(_)),
    )
      .decodeOnly
      .withContext("non-object data")

  def apply(data: ByteVector): Attempt[List[Parsed]] =
    decoder.decode(data.bits)
      .map(a => List(a.value))
      .mapErr(e => Err(s"parsing raw: ${e.messageWithContext}\n${Codecs.sanitize(data)}\n"))
}

object ParseObjects
{
  def extractObjectStream(stream: Eval[Attempt[BitVector]]): Prim => Option[Attempt[ObjectStream]] = {
    case Prim.tpe("ObjStm", _) =>
      Some(
        stream
          .value
          .flatMap(ObjectStream.Codec_ObjectStream.complete.decode)
          .map(_.value)
      )
    case _ =>
      None
  }

  def extractStreamObjects(stream: Eval[Attempt[BitVector]])(data: Prim): Attempt[Option[NonEmptyList[Parsed]]] =
    extractObjectStream(stream)(data) match {
      case Some(os) =>
        os
        .map(_.objs.map(Parsed.StreamObject(_)))
        .map(NonEmptyList.fromList)
      case None =>
        Attempt.successful(None)
    }

  val streamEndMarker: ByteVector =
    ByteVector("endstream".getBytes)

  def streamLength(dict: Prim): Attempt[Long] =
    Prim.Dict.number("Length")(dict).map(_.toLong)

  def endstreamIndex(bytes: ByteVector): Attempt[Long] =
    bytes.indexOfSlice(streamEndMarker) match {
      case i if i >= 0 => Attempt.successful(i)
      case _ => Attempt.failure(Err.InsufficientBits(0, bytes.bits.size, List("no stream end position found")))
    }

  def stripStreamEndMarker(bits: BitVector): Attempt[BitVector] =
    endstreamIndex(bits.bytes).map(i => Codecs.stripNewline(bits.bytes.take(i)).bits)

  def streamStartMarkerDecoder: Decoder[Unit] = {
    import scodec.codecs.choice
    Codecs.constantString("stream") <~ choice(Codecs.crlf, Codecs.lf)
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

  def outputObjs(obj: Obj, stream: Parsed.Stream, text: ByteVector, rawStream: BitVector)
  : Option[NonEmptyList[Parsed]] => List[Parsed] = {
    case Some(streamObjs) =>
      streamObjs.toList
    case None =>
      List(Parsed.IndirectObj(obj, Some(stream), text ++ rawStream.bytes))
  }

  def contentObject(text: ByteVector, rawStream: BitVector): Attempt[List[Parsed]] =
    for {
      obj <- parseObject(text)
      uncompressed <- uncompressStream(obj.data)(rawStream)
      streamObjs <- extractStreamObjects(uncompressed.data)(obj.data)
    } yield outputObjs(obj, uncompressed, text, rawStream)(streamObjs)

  def dataObject(text: ByteVector): Attempt[Parsed] =
    parseObject(text)
      .map(Parsed.IndirectObj(_, None, text))

  private[this] def multi(message: String)(parsed: Attempt[List[Parsed]]): Stream[IO, Parsed] =
    StreamUtil.attemptStream(message)(parsed)
      .flatMap(Stream.emits)

  def processChunk: ObjectChunk => Stream[IO, Parsed] = {
    case ObjectChunk.Text(bytes) =>
      multi("parsing non-object")(ParseNonObject(bytes))
    case ObjectChunk.DataObject(text) =>
      StreamUtil.attemptStream("parsing data object")(dataObject(text))
    case ObjectChunk.ContentObject(text, stream) =>
      val parsed =
        contentObject(text, stream)
          .mapErr(e => Err(s"${e.messageWithContext}; when parsing ${Codecs.sanitize(text)}"))
      multi("parsing content object")(parsed)
  }

  def pipe: Pipe[IO, ObjectChunk, Parsed] =
    _.flatMap(processChunk)
}
