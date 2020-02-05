package fs2
package pdf

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}
import scodec.interop.cats.AttemptMonadErrorInstance

case class Page(index: Obj.Index, data: Prim.Dict)

object Page
{
  object fromStreamPrim
  {
    def unapply(streamData: (Long, Prim)): Option[Page] =
    streamData match {
      case (number, Prim.tpe("Page", data)) =>
        Some(Page(Obj.Index(number, 0), data))
      case _ =>
        None
    }
  }
}

case class PageDir(index: Obj.Index, data: Prim.Dict)

case class FontResource(index: Obj.Index, data: Prim.Dict)

case class IndirectArray(index: Obj.Index, data: Prim.Array)

case class Image(obj: Obj, codec: Image.Codec, stream: Parsed.Stream)

object Image
{
  sealed trait Codec

  object Codec
  {
    case object Jpg
    extends Codec

    case object Ccitt
    extends Codec

    def extension: Codec => String = {
      case Jpg => "jpg"
      case Ccitt => "tiff"
    }
  }
}

sealed trait PdfObj

object PdfObj
{
  case class Indirect(obj: IndirectObj)
  extends PdfObj

  case class Stream(obj: Obj)
  extends PdfObj

  case class Unparsable(index: Obj.Index, data: ByteVector)
  extends PdfObj
}

case class Pdf(objs: NonEmptyList[PdfObj], xrefs: NonEmptyList[Xref])

case class ValidatedPdf(pdf: Pdf, errors: ValidatedNel[String, Unit])

object Pdf
{
  def objectNumbersRaw(numbers: List[Long]): Pipe[IO, Byte, (Obj, Option[BitVector])] =
    _
      .through(StreamParser.objects(Log.noop))
      .collect {
        case Parsed.IndirectObj(o @ Obj(Obj.Index(n, _), _), s, _) if numbers.contains(n) =>
          (o, s.map(_.original))
      }

  def objectNumbers(numbers: List[Long]): Pipe[IO, Byte, (Obj, Option[BitVector])] =
    _
      .through(StreamParser.objects(Log.noop))
      .collect {
        case Parsed.IndirectObj(o @ Obj(Obj.Index(n, _), _), s, _) if numbers.contains(n) =>
          s.traverse(a => StreamUtil.attemptStream(s"stream of object $n")(a.data.value))
            .map(a => (o, a))
      }
      .flatten

  def pageNumber(page: Int): Pipe[IO, Byte, Long] =
    _
      .through(StreamParser.analyzed(Log.noop))
      .collect {
        case Analyzed.PageDir(PageDir(_, pages)) =>
          Prim.Dict.array("Kids")(pages).map(_.lift(page))
      }
      .collect { case Attempt.Successful(Some(Prim.Ref(a, _))) => a }
      .head

  def pageObject(page: Int): Pipe[IO, Byte, (Obj, Option[BitVector])] =
    in =>
      pageNumber(page)(in)
        .map(List(_))
        .flatMap(objectNumbers(_)(in))

  def rawStreamOfObjs(numbers: List[Long]): Pipe[IO, Byte, BitVector] =
    objectNumbersRaw(numbers)(_)
      .collect { case (_, Some(bits)) => bits }

  def rawStreamOfObj(number: Long): Pipe[IO, Byte, BitVector] =
    rawStreamOfObjs(List(number))
      .andThen(_.take(1))

  def streamOfObjs(number: List[Long]): Pipe[IO, Byte, BitVector] =
    objectNumbers(number)(_)
      .collect { case (_, Some(bits)) => bits }

  def streamTextOfObjs(numbers: List[Long]): Pipe[IO, Byte, String] =
    streamOfObjs(numbers)(_).flatMap { bits =>
      StreamUtil.attemptStream(s"text stream of object $numbers")(scodec.codecs.utf8.decode(bits).map(_.value))
    }

  def dictOfPage(page: Int): Pipe[IO, Byte, Prim.Dict] =
    pageObject(page)(_)
      .collect { case (Obj(_, data @ Prim.Dict(_)), _) => data }

  def streamsOfPage(page: Int): Pipe[IO, Byte, String] =
    in =>
      dictOfPage(page)(in)
        .flatMap { data =>
          Prim.Dict.array("Contents")(data)
            .map(_.collect { case Prim.Ref(num, _) => num })
            .map(refs => streamTextOfObjs(refs)(in))
            .getOrElse(Stream.empty)
        }

  def dictOfObj(number: Long): Pipe[IO, Byte, Map[String, Prim]] =
    objectNumbers(List(number))(_)
      .collect { case (Obj(_, Prim.Dict(data)), _) => data }

  object Assemble
  {
    case class AssemblyState(objs: List[PdfObj], xrefs: List[Xref], errors: List[String])
    {
      def obj(o: PdfObj): AssemblyState =
        copy(objs = o :: objs)

      def xref(x: Xref): AssemblyState =
        copy(xrefs = x :: xrefs)

      def error(e: String): AssemblyState =
        copy(errors = e :: errors)
    }

    def processParsed(state: AssemblyState): Parsed => AssemblyState = {
      case Parsed.IndirectObj(obj @ Obj.tpe("XRef", data), Some(stream), _) =>
          XrefStream(data)(stream) match {
          case Attempt.Successful(XrefStream(tables, trailer)) =>
            state.xref(Xref(tables, trailer, 0))
          case Attempt.Failure(cause) =>
            state.error(s"broken xref stream for ${obj.index}: ${cause.messageWithContext}")
        }
      case Parsed.IndirectObj(obj, stream, _) =>
        stream.traverse(_.data.value) match {
          case Attempt.Successful(s) =>
            state.obj(PdfObj.Indirect(IndirectObj(obj.index, obj.data, s)))
          case Attempt.Failure(cause) =>
            state.error(s"broken stream for ${obj.index}: ${cause.messageWithContext}")
        }
      case Parsed.Raw(bytes) =>
        AnalyzeObject.parseXref(bytes) match {
          case Analyzed.Xref(xref, _) => state.xref(xref)
          case Analyzed.Garbage(_) =>
            Version.codec.decode(bytes.bits)
              .fold(_ => state.error(s"garbage: $bytes"), _ => state)
          case _ =>
            state
        }
      case Parsed.StreamObject(obj) =>
        state.obj(PdfObj.Stream(obj))
      case Parsed.Unparsable(index, _) =>
        state.error(s"unparsable object: $index")
    }

    def processParsedPull(state: AssemblyState)(parsed: Parsed): Pull[IO, Nothing, AssemblyState] =
      Pull.pure(processParsed(state)(parsed))

    def consPdf(objs: List[PdfObj], xrefs: List[Xref]): Validated[String, Pdf] =
      (NonEmptyList.fromList(objs.reverse), NonEmptyList.fromList(xrefs.reverse)) match {
        case (Some(o), Some(x)) =>
          Validated.Valid(Pdf(o, x))
        case (None, _) =>
          Validated.Invalid("no objects in pdf")
        case (_, None) =>
          Validated.Invalid("no xrefs in pdf")
      }

    def validateErrors(errors: List[String]): ValidatedNel[String, Unit] =
      NonEmptyList.fromList(errors)
        .fold(Validated.validNel[String, Unit](()))(Validated.invalid(_))

    def pull(parsed: Stream[IO, Parsed]): Pull[IO, ValidatedNel[String, ValidatedPdf], Unit] =
      StreamUtil.pullState(processParsedPull)(parsed)(AssemblyState(Nil, Nil, Nil))
        .flatMap {
          case AssemblyState(objs, xrefs, errors) =>
            Pull.output1(consPdf(objs, xrefs).toValidatedNel.map(ValidatedPdf(_, validateErrors(errors))))
        }

    def apply(parsed: Stream[IO, Parsed]): IO[ValidatedNel[String, ValidatedPdf]] =
      pull(parsed)
        .stream
        .compile
        .last
        .map(_.getOrElse(Validated.invalidNel("no output from Assemble")))
  }
}
