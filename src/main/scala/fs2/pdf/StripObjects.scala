package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err}
import scodec.bits.{BitVector, ByteVector}

case class Sized[A](size: Long, value: A)

sealed trait ObjectChunk

object ObjectChunk
{
  case class Text(bytes: ByteVector)
  extends ObjectChunk

  case class Object(size: Long, text: ByteVector, stream: Option[BitVector])
  extends ObjectChunk

  def text(bytes: ByteVector): ObjectChunk =
    ObjectChunk.Text(bytes)
}

object StripObjects
{
  val objectStartMarker: ByteVector =
    ByteVector(" obj".getBytes)

  val streamStartMarker: ByteVector =
    ByteVector("stream".getBytes)

  def version: Decoder[RawPdfChunk] =
    Codecs.line("version").map(RawPdfChunk.Rest(_))

  def comment: Decoder[RawPdfChunk] =
    Codecs.line("comment").emap { bytes =>
      if (bytes.headOption.contains('%')) Attempt.successful(RawPdfChunk.Rest(bytes))
      else Attempt.failure(Err("not a comment"))
    }

  def void[A](codec: Codec[A]): Codec[Unit] =
    codec.map(_ => ()).decodeOnly

  val newlineMarkers: List[Byte] =
    List(Codecs.linuxNewlineByte, Codecs.macosNewlineByte)

  def findNewlineLeft(bytes: ByteVector, start: Long): Option[Long] = {
    @annotation.tailrec
    def spin(index: Long): Option[Long] =
      bytes.lift(index) match {
        case None => None
        case Some(byte) =>
          if (newlineMarkers.contains(byte)) Some(index)
          else if (index == 0) Some(0)
          else spin(index - 1)
      }
    spin(start)
  }

  def findNewlineLeftOrFail(message: String)(bytes: ByteVector)(start: Long): Attempt[Long] =
    findNewlineLeft(bytes, start) match {
      case Some(index) => Attempt.successful(index)
      case None => Attempt.failure(Err(message))
    }

  def splitObjectStream(bytes: ByteVector, offset: Long): ObjectChunk =
    bytes.indexOfSlice(streamStartMarker, offset) match {
      case i if i >= 0 =>
        val indexAfter = i + streamStartMarker.size
        if (bytes.lift(indexAfter).exists(newlineMarkers.contains))
          ObjectChunk.Object(bytes.size, bytes.take(i), Some(bytes.drop(i).bits))
        else
          splitObjectStream(bytes, indexAfter)
      case _ =>
        ObjectChunk.Object(bytes.size, bytes, None)
    }

  def splitObjectText(index: Long, bytes: ByteVector): (List[ObjectChunk], ByteVector) =
    if (index <= 0) (Nil, bytes)
    else (
      List(ObjectChunk.Text(bytes.take(index + 1))),
      bytes.drop(index + 1)
    )

  def stripTextPrefix: Decoder[List[ObjectChunk]] =
    Decoder { bits =>
      val bytes = bits.bytes
      for {
        objKeyword <- bytes.indexOfSlice(objectStartMarker) match {
          case i if i >= 0 => Attempt.Successful(i)
          case _ => Attempt.failure(Err("no object start marker in ObjectChunk"))
        }
        objStart <- findNewlineLeftOrFail("no object start position found in ObjectChunk")(bytes)(objKeyword)
        (text, obj) = splitObjectText(objStart, bytes)
      } yield DecodeResult(text ++ List(splitObjectStream(obj, 0)), BitVector.empty)
  }

  def processChunk(log: Log): RawPdfChunk => Stream[IO, ObjectChunk] = {
    case RawPdfChunk.Rest(text) =>
      Stream(ObjectChunk.text(text))
    case RawPdfChunk.Object(bytes) =>
      stripTextPrefix.decode(bytes.bits) match {
        case Attempt.Successful(DecodeResult(chunks, remainder)) if remainder.isEmpty =>
          Stream.emits(chunks)
        case Attempt.Successful(a) =>
          Stream.eval(log.error(s"leftovers when stripping object chunk: $a")).drain
        case Attempt.Failure(cause) =>
          Stream.eval(log.error(s"failed to strip object chunk: $cause")).drain
      }
  }

  // Remove non-object text from object elements that come from the imprecise cleaving method.
  // Only the `endobj` lines are used as indicators, so comments, xref, trailer and startxref sections are still
  // prefixed to objects.
  def pipe(log: Log): Pipe[IO, RawPdfChunk, ObjectChunk] =
    _.flatMap(processChunk(log))
}
