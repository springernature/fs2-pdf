package fs2
package pdf

/**
  * Helper data type for encoding streams of indirect objects.
  */
sealed trait Part[+A]

object Part
{
  case class Obj(obj: IndirectObj)
  extends Part[Nothing]

  case class Meta[A](meta: A)
  extends Part[A]

  case class Version(version: pdf.Version)
  extends Part[Nothing]
}
