package fs2
package pdf

import scodec.bits.ByteVector

sealed trait Part[+A]

object Part
{
  case class Obj(obj: IndirectObj)
  extends Part[Nothing]

  case class Unparsable(index: pdf.Obj.Index, data: ByteVector)
  extends Part[Nothing]

  case class Meta[A](meta: A)
  extends Part[A]

  case class Version(version: pdf.Version)
  extends Part[Nothing]
}
