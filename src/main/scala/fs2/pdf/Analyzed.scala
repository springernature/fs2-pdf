package fs2
package pdf

import scodec.bits.{BitVector, ByteVector}

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

  case class FontResources(res: FontResource)
  extends Analyzed

  case class IndirectArray(array: pdf.IndirectArray)
  extends Analyzed

  case class Xref(xref: pdf.Xref)
  extends Analyzed

  case class XrefStream(xref: pdf.XrefStream)
  extends Analyzed

  case class StartXref(startxref: Long)
  extends Analyzed

  case class Version(version: pdf.Version)
  extends Analyzed

  case class Keep(obj: Obj, stream: Option[BitVector])
  extends Analyzed

  case class KeepUnparsable(index: Obj.Index, data: ByteVector)
  extends Analyzed
}
