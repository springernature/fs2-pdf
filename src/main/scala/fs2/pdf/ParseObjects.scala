package fs2
package pdf

import cats.Eval
import cats.data.NonEmptyList
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}
import scodec.{Attempt, Codec, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.interop.cats._
import scodec.stream.StreamDecoder
import shapeless.{:+:, CNil}

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

  val streamEndMarker: BitVector =
    BitVector("endstream".getBytes)

  def streamLength(dict: Prim): Attempt[Long] =
    Prim.Dict.number("Length")(dict).map(_.toLong)

  def endstreamIndex(bits: BitVector): Attempt[Long] =
    bits.indexOfSlice(streamEndMarker) match {
      case i if i >= 0 => Attempt.successful(i)
      case _ => Attempt.failure(Err.InsufficientBits(0, bits.size, List("no stream end position found")))
    }

  def stripStreamEndMarker(bits: BitVector): Attempt[BitVector] =
    endstreamIndex(bits).map(i => Codecs.stripNewlineBits(bits.take(i)))

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
      streamObjs <- uncompressed.map(_.data).flatTraverse(extractStreamObjects(_)(obj.data))
    } yield outputObjs(obj, uncompressed, text, rawStream)(streamObjs)

  def processChunk: ObjectChunk => Stream[IO, Parsed] = {
    case ObjectChunk.Text(bytes) =>
      Stream.emit(Parsed.Raw(bytes))
    case ObjectChunk.Object(_, text, stream) =>
      processObject(text, stream) match {
        case Attempt.Successful(parsed) =>
          Stream.emits(parsed)
        case Attempt.Failure(cause) =>
          StreamUtil.failStream(s"${cause.messageWithContext}; when parsing ${Codecs.sanitize(text)}")
      }
  }

  def pipe: Pipe[IO, ObjectChunk, Parsed] =
    _.flatMap(processChunk)
}

sealed trait TopLevel

object TopLevel
{
  case class IndirectObj(obj: pdf.IndirectObj)
  extends TopLevel

  case class Version(version: pdf.Version)
  extends TopLevel

  case class Comment(data: pdf.Comment)
  extends TopLevel

  case class Xref(version: pdf.Xref)
  extends TopLevel

  /**
    * The coproduct type must be specified explicitly because the macro will order the types alphabetically, making
    * Comment supersede Version.
    *
    * @return [[scodec.Decoder]] for [[TopLevel]]
    */
  def Decoder_TopLevel: Decoder[TopLevel] =
    Codec.coproduct[IndirectObj :+: Version :+: Comment :+: Xref :+: CNil].choice.as[TopLevel]

  def streamDecoder: Decoder[TopLevel] =
    Decoder(bits => Decoder_TopLevel.decode(bits).mapErr(e => Err.InsufficientBits(0, 0, e.context)))

  def pipe: Pipe[IO, Byte, TopLevel] =
    StreamDecoder.many(streamDecoder).toPipeByte
}

sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, stream: Eval[Attempt[BitVector]])
  extends Decoded

  case class Meta(xrefs: NonEmptyList[Xref], version: Version)
  extends Decoded
}

object Decode
{
  case class State(xrefs: List[Xref], version: Option[Version])

  def decodeObjectStream(stream: Eval[Attempt[BitVector]])(data: Prim): Option[Attempt[List[Decoded]]] =
    ParseObjects.extractObjectStream(stream)(data)
      .map(os => os.map(_.objs).map(_.map(Decoded.DataObj(_))))

  def analyzeStream(index: Obj.Index, data: Prim)(stream: Eval[Attempt[BitVector]]): Attempt[List[Decoded]] =
    decodeObjectStream(stream)(data)
      .getOrElse(Attempt.successful(List(Decoded.ContentObj(Obj(index, data), stream))))

  def contentObj(index: Obj.Index, data: Prim, stream: BitVector): Pull[IO, Decoded, Unit] =
    StreamUtil.attemptPullWith("extract stream objects")(
      analyzeStream(index, data)(ParseObjects.uncompressStrippedStream(stream)(data))
    )(a => Pull.output(Chunk.seq(a)))

  def pullTopLevel(state: State): TopLevel => Pull[IO, Decoded, State] = {
    case TopLevel.IndirectObj(IndirectObj(index, data, Some(stream))) =>
      contentObj(index, data, stream).as(state)
    case TopLevel.IndirectObj(IndirectObj(index, data, None)) =>
      Pull.output1(Decoded.DataObj(Obj(index, data))).as(state)
    case TopLevel.Version(version) =>
      Pull.pure(state.copy(version = Some(version)))
    case TopLevel.Xref(xref) =>
      Pull.pure(state.copy(xrefs = xref :: state.xrefs))
    case TopLevel.Comment(_) =>
      Pull.pure(state)
  }

  def decodeTopLevelPull(in: Stream[IO, TopLevel]): Pull[IO, Decoded, Unit] =
    StreamUtil.pullState(pullTopLevel)(in)(State(Nil, None))
      .flatMap {
        case State(h :: t, Some(version)) =>
          Pull.output1(Decoded.Meta(NonEmptyList(h, t), version))
        case _ =>
          StreamUtil.failPull("no xref or version")
      }

  def decodeTopLevelPipe: Pipe[IO, TopLevel, Decoded] =
    decodeTopLevelPull(_).stream

  def decoded: Pipe[IO, Byte, Decoded] =
    TopLevel.pipe.andThen(decodeTopLevelPipe)
}
