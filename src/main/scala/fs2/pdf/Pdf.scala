package fs2
package pdf

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.effect.IO
import fs2.{Pipe, Pull, Stream}
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}

sealed trait PdfObj

object PdfObj
{
  case class Content(obj: IndirectObj)
  extends PdfObj

  case class Data(obj: Obj)
  extends PdfObj

  case class Unparsable(index: Obj.Index, data: ByteVector)
  extends PdfObj
}

case class Pdf(objs: NonEmptyList[PdfObj], xrefs: NonEmptyList[Xref], trailer: Trailer)

case class ValidatedPdf(pdf: Pdf, errors: ValidatedNel[String, Unit])

object Pdf
{
  def objectNumbersRaw(numbers: List[Long]): Pipe[IO, Byte, (Obj, Option[BitVector])] =
    _
      .through(StreamParser.topLevel)
      .collect {
        case TopLevel.IndirectObj(IndirectObj(index @ Obj.Index(n, _), data, s)) if numbers.contains(n) =>
          (Obj(index, data), s)
      }

  def objectNumbers(numbers: List[Long]): Pipe[IO, Byte, (Obj, Option[BitVector])] =
    _
      .through(StreamParser.decode(Log.noop))
      .collect {
        case Decoded.ContentObj(obj @ Obj(Obj.Index(n, _), _), _, stream) if numbers.contains(n) =>
          StreamUtil.attemptStream(s"stream of object $n")(stream.exec)
            .map(a => (obj, Some(a)))
        case Decoded.DataObj(obj @ Obj(Obj.Index(n, _), _)) if numbers.contains(n) =>
          Stream((obj, None))
      }
      .flatten

  def pageNumber(page: Int): Pipe[IO, Byte, Long] =
    _
      .through(StreamParser.elements(Log.noop))
      .collect {
        case Element.Data(_, Element.DataKind.Pages(Pages(_, _, kids, _))) =>
          kids.toList.lift(page).map(_.number)
      }
      .collect { case Some(a) => a }
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

      def error(e: String): AssemblyState =
        copy(errors = e :: errors)
    }

    def processParsed(state: AssemblyState): Decoded => AssemblyState = {
      case Decoded.ContentObj(obj, _, stream) =>
        stream.exec match {
          case Attempt.Successful(s) =>
            state.obj(PdfObj.Content(IndirectObj(obj.index, obj.data, Some(s))))
          case Attempt.Failure(cause) =>
            state.error(s"broken stream for ${obj.index}: ${cause.messageWithContext}")
        }
      case Decoded.Meta(xrefs, _, _) =>
        state.copy(xrefs = xrefs.toList)
      case Decoded.DataObj(obj) =>
        state.obj(PdfObj.Data(obj))
      case _ =>
        state
    }

    def processParsedPull(state: AssemblyState)(parsed: Decoded): Pull[IO, Nothing, AssemblyState] =
      Pull.pure(processParsed(state)(parsed))

    def consPdf(objs: List[PdfObj], xrefs: List[Xref]): Validated[String, Pdf] =
      (NonEmptyList.fromList(objs.reverse), NonEmptyList.fromList(xrefs.reverse)) match {
        case (Some(o), Some(x)) =>
          Validated.Valid(Pdf(o, x, Trailer.sanitize(x.map(_.trailer))))
        case (None, _) =>
          Validated.Invalid("no objects in pdf")
        case (_, None) =>
          Validated.Invalid("no xrefs in pdf")
      }

    def validateErrors(errors: List[String]): ValidatedNel[String, Unit] =
      NonEmptyList.fromList(errors)
        .fold(Validated.validNel[String, Unit](()))(Validated.invalid(_))

    def pull(parsed: Stream[IO, Decoded]): Pull[IO, ValidatedNel[String, ValidatedPdf], Unit] =
      StreamUtil.pullState(processParsedPull)(parsed)(AssemblyState(Nil, Nil, Nil))
        .flatMap {
          case AssemblyState(objs, xrefs, errors) =>
            Pull.output1(consPdf(objs, xrefs).toValidatedNel.map(ValidatedPdf(_, validateErrors(errors))))
        }

    def apply(parsed: Stream[IO, Decoded]): IO[ValidatedNel[String, ValidatedPdf]] =
      pull(parsed)
        .stream
        .compile
        .last
        .map(_.getOrElse(Validated.invalidNel("no output from Assemble")))
  }
}
