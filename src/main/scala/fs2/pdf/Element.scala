package fs2
package pdf

import cats.effect.IO
import scodec.Attempt
import scodec.bits.{BitVector, ByteVector}

/**
  * MediaBox is a mandatory field in [[Page]] dictionaries that specifies the page's geometry.
  */
case class MediaBox(x: BigDecimal, y: BigDecimal, w: BigDecimal, h: BigDecimal)

/**
  * Convenience record for Page objects with mandatory [[MediaBox]]
  *
  * @param mediaBox page geometry
  */
case class Page(index: Obj.Index, data: Prim.Dict, mediaBox: MediaBox)

/**
  * Helpers for extracting a [[Page]] from an [[IndirectObj]] or its components.
  */
object Page
{
  def fromData(index: Obj.Index): Prim => Attempt[Page] = {
    case Prim.tpe("Page", data) =>
      Prim.Dict.path("MediaBox")(data) {
        case Prim.Array(List(Prim.Number(x), Prim.Number(y), Prim.Number(w), Prim.Number(h))) =>
          Page(index, data, MediaBox(x, y, w, h))
      }
    case _ =>
      Scodec.fail("not a Page object")
  }

  object obj
  {
    def unapply(obj: IndirectObj): Option[Page] =
      fromData(obj.obj.index)(obj.obj.data).toOption
  }
}

/**
  * Convenience record for Pages objects, which contain references to other Pages and Page objects forming the page
  * tree, with mandatory Kids field.
  *
  * @param kids children of this Pages object, forming a balanced tree with page numbers going left-to-right
  * @param root whether this object is the root referenced in the trailer
  */
case class Pages(index: Obj.Index, data: Prim.Dict, kids: List[Prim.Ref], root: Boolean)

/**
  * Helpers for extracting a [[Pages]] record from an [[IndirectObj]] or its components.
  */
object Pages
{
  def fromData(index: Obj.Index): Prim => Attempt[Pages] = {
    case Prim.tpe("Pages", data) =>
      Prim.Dict.path("Kids")(data) {
        case Prim.refs(kids) => Pages(index, data, kids, !data.data.contains("Parent"))
      }
    case _ =>
      Scodec.fail("not a Pages object")
  }

  object obj
  {
    def unapply(obj: IndirectObj): Option[Pages] =
      fromData(obj.obj.index)(obj.obj.data).toOption
  }
}

/**
  * Marker record for data objects containing the /Font key, which may be used as indirect objects in page objects as
  * resource dict.
  *
  */
case class FontResource(index: Obj.Index, data: Prim.Dict)

/**
  * Convenience record for indirect objects containing a sole array.
  *
  * @param data the array with specialized type
  */
case class IndirectArray(index: Obj.Index, data: Prim.Array)

/**
  * An image with codec information, JPG or CCITT (TIFF), bundled with its uncompressed stream.
  *
  * @param stream uncompressed in case of a predictor, which may be used for CCITT
  * @param codec either JPG or CCITT
  */
case class Image(data: Prim.Dict, stream: Uncompressed, codec: Image.Codec)

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

/**
  * Abstracts different kinds of PDF objects semantically.
  * To be extended by consumers of the library by creating a new ADT and matching the /Type entry of General instances.
  * Two levels of specialization are used, for content (with stream) and data objects.
  */
sealed trait Element

object Element
{
  sealed trait DataKind

  object DataKind
  {
    case object General
    extends DataKind

    case class Page(page: pdf.Page)
    extends DataKind

    case class Pages(pages: pdf.Pages)
    extends DataKind

    case class Array(data: Prim.Array)
    extends DataKind

    case class FontResource(res: pdf.FontResource)
    extends DataKind

    case class Info(data: Prim.Dict)
    extends DataKind
  }

  case class Data(obj: Obj, kind: DataKind)
  extends Element

  sealed trait ContentKind

  object ContentKind
  {
    case object General
    extends ContentKind

    case class Image(image: pdf.Image)
    extends ContentKind
  }

  case class Content(obj: Obj, rawStream: BitVector, stream: Uncompressed, kind: ContentKind)
  extends Element

  case class Meta(trailer: Option[Trailer], version: Option[Version])
  extends Element

  object obj
  {
    def unapply(element: Element): Option[IndirectObj] =
      element match {
        case Data(obj, _) => Some(IndirectObj(obj, None))
        case Content(obj, stream, _, _) => Some(IndirectObj(obj, Some(stream)))
        case Meta(_, _) => None
      }
  }

  private[pdf]
  def part: RewriteState[Unit] => Element => (List[Part[Trailer]], RewriteState[Unit]) =
    state => {
      case obj(obj) =>
        (List(Part.Obj(obj)), state)
      case Element.Meta(trailer, _) =>
        (Nil, state.copy(trailer = trailer))
    }

  /**
    * @return a [[Pipe]] that converts elements back into [[Part]], which is encodable in [[WritePdf]].
    */
  def parts: Pipe[IO, Element, Part[Trailer]] =
    Rewrite.simpleParts(())(part)(Rewrite.noUpdate)

  /**
    * @return a [[Pipe]] that encodes [[Element]] back to bytes.
    */
  def encode: Pipe[IO, Element, ByteVector] =
    Rewrite.simple(())(part)(Rewrite.noUpdate)
}
