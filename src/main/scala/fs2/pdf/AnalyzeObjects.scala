package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.{Attempt, Codec, DecodeResult}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.fallback

sealed trait Analyzed

object Analyzed
{
  case class Image(image: pdf.Image)
  extends Analyzed

  case class Page(page: pdf.Page)
  extends Analyzed

  case class PageDir(dir: pdf.PageDir)
  extends Analyzed

  case class PageNumbers(numbers: List[Long])
  extends Analyzed

  case class FontResources(res: FontResource, nums: List[Long])
  extends Analyzed

  case class Fonts(nums: List[Long])
  extends Analyzed

  case class IndirectArray(array: pdf.IndirectArray)
  extends Analyzed

  case class Xref(xref: pdf.Xref, data: Option[ByteVector])
  extends Analyzed

  case class XrefStream(xref: pdf.XrefStream)
  extends Analyzed

  case class StartXref(startxref: Long)
  extends Analyzed

  case class Keep(obj: Obj, stream: Option[BitVector])
  extends Analyzed

  case class KeepUnparsable(index: Obj.Index, data: ByteVector)
  extends Analyzed

  case class Garbage(data: ByteVector)
  extends Analyzed

  case object Linearized
  extends Analyzed
}

object AnalyzeStream
{
  object SupportedCodec
  {
    def isFilter(filter: String)(data: Prim.Dict): Boolean =
      Prim.containsName("Filter")(filter)(data)

    def unapply(data: Prim.Dict): Option[Image.Codec] =
      if (isFilter("DCTDecode")(data)) Some(Image.Codec.Jpg)
      else
        if (isFilter("CCITTFaxDecode")(data)) Some(Image.Codec.Ccitt)
        else None
  }

  def apply(stream: Parsed.Stream): Obj => Attempt[Analyzed] = {
    case obj @ Obj.subtype("Image", SupportedCodec(codec)) =>
      Attempt.successful(Analyzed.Image(Image(obj, codec, stream)))
    case Obj.tpe("XRef", data) =>
      XrefStream(data)(stream).map(Analyzed.XrefStream(_))
    case obj =>
      Attempt.successful(Analyzed.Keep(obj, Some(stream.original)))
  }
}

object AnalyzeObject
{
  def metadataObj: Obj => Analyzed = {
    case Obj(index, Prim.tpe("Page", data)) =>
      Analyzed.Page(Page(index, data))
    case obj @ Obj.tpe("Pages", data) if data.data.contains("Parent") =>
      Analyzed.Keep(obj, None)
    case Obj(index, Prim.tpe("Pages", data)) =>
      Analyzed.PageDir(PageDir(index, data))
    case Obj(index, Prim.fontResources(data, nums)) =>
      Analyzed.FontResources(FontResource(index, data), nums)
    case Obj(_, Prim.linearization()) =>
      Analyzed.Linearized
    case Obj(index, data @ Prim.Array(_)) =>
      Analyzed.IndirectArray(IndirectArray(index, data))
    case obj =>
      Analyzed.Keep(obj, None)
  }

  def parseRewritableObj(obj: Obj, stream: Option[Parsed.Stream]): Attempt[Analyzed] =
    stream.fold(Attempt.successful(metadataObj(obj)))(AnalyzeStream(_)(obj))

  def collectObjStats: Obj => Attempt[Option[Analyzed]] = {
    case Obj(_, Prim.tpe("Pages", data)) =>
      Attempt.successful(
        Prim.Dict.path("Kids")(data) { case Prim.Array(kids) => kids.collect { case Prim.Ref(n, _) => n } }
          .toOption
          .map(Analyzed.PageNumbers(_))
        )
    case Obj(_, Prim.fonts(nums)) =>
      Attempt.successful(Some(Analyzed.Fonts(nums)))
    case _ =>
      Attempt.successful(None)
  }

  def parseObject(obj: Obj, stream: Option[Parsed.Stream]): Attempt[List[Analyzed]] =
    for {
      rw <- parseRewritableObj(obj, stream)
      stats <- stream.fold(collectObjStats(obj))(_ => Attempt.successful(None))
    } yield rw :: stats.toList

  def xrefDecoder: Codec[Either[Long, Xref]] =
    fallback(Xref.startxref, Xref.Codec_Xref)

  def parseXref(data: ByteVector): Analyzed =
    xrefDecoder.decode(Codecs.removeComments(data).bits) match {
      case Attempt.Successful(DecodeResult(Right(xref), _)) =>
        Analyzed.Xref(xref, Some(data))
      case Attempt.Successful(DecodeResult(Left(startxref), _)) =>
        Analyzed.StartXref(startxref)
      case Attempt.Failure(_) =>
        Analyzed.Garbage(data)
    }

  def parsed: Parsed => Attempt[List[Analyzed]] = {
    case Parsed.Raw(data) =>
      Attempt.successful(parseXref(data)).map(List(_))
    case Parsed.IndirectObj(obj, stream, _) =>
      parseObject(obj, stream)
    case Parsed.StreamObject(obj) =>
      parseObject(obj, None)
    case Parsed.Unparsable(index, data) =>
      Attempt.successful(List(Analyzed.KeepUnparsable(index, data)))
  }
}

object AnalyzeObjects
{
  def analyze(parsed: Parsed): Stream[IO, List[Analyzed]] =
    StreamUtil.attemptStream("failed to analyze object")(AnalyzeObject.parsed(parsed))

  def analyzed: Pipe[IO, Parsed, Analyzed] =
    _
      .flatMap(analyze)
      .flatMap(Stream.emits)
}
