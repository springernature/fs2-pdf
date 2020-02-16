package fs2
package pdf

import cats.data.NonEmptyList
import cats.effect.IO
import fs2.{Pipe, Stream}
import scodec.bits.BitVector

/**
  * Abstract representation of basic components of a PDF file.
  *
  * Objects come in two syntactic variants: either as top level indirect objects, or contained in an indirect object's
  * stream. This type unifies the two concepts, but splits them in those with streams ([[ContentObj]]) and those without
  * ([[DataObj]]).
  * The [[Meta]] variant carries all non-object information.
  * [[ContentObj]]s additionally provide lazily evaluated uncompressed versions of the object's stream.
  */
sealed trait Decoded

object Decoded
{
  case class DataObj(obj: Obj)
  extends Decoded

  case class ContentObj(obj: Obj, rawStream: BitVector, stream: Uncompressed)
  extends Decoded

  case class Meta(xrefs: NonEmptyList[Xref], trailer: Trailer, version: Version)
  extends Decoded

  /**
    * Trivially turn [[Decoded]] back into [[IndirectObj]]s.
    *
    * @return [[Pipe]] that turns [[Decoded]]s into [[IndirectObj]]
    */
  def objects: Pipe[IO, Decoded, IndirectObj] =
    _.flatMap {
      case Decoded.DataObj(obj) =>
        Stream(IndirectObj(obj, None))
      case Decoded.ContentObj(obj, _, stream) =>
        StreamUtil.attemptStream("Decode.indirectObjs")(stream.exec)
          .map(Some(_))
          .map(IndirectObj(obj, _))
      case Decoded.Meta(_, _, _) =>
        fs2.Stream.empty
    }

  /**
    * State updating function for [[Rewrite.simpleParts]] that only collects the trailer and generalizes objects back
    * to [[IndirectObj]].
    *
    * @return parts and updated state
    */
  def part: RewriteState[Unit] => Decoded => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case Decoded.Meta(_, trailer, _) =>
        (Nil, state.copy(trailer = Some(trailer)))
      case Decoded.DataObj(obj) =>
        (List(Part.Obj(IndirectObj(obj, None))), state)
      case Decoded.ContentObj(obj, rawStream, _) =>
        (List(Part.Obj(IndirectObj(obj, Some(rawStream)))), state)
      case _ =>
        (Nil, state)
    }

  /**
    * Trivially turn [[Decoded]] back into encodable [[Part]]s.
    *
    * @return [[Pipe]] that turns [[Decoded]]s into [[Part]]
    */
  def parts: Pipe[IO, Decoded, Part[Trailer]] =
    Rewrite.simpleParts(())(part)(update => Part.Meta(update.trailer))
}
