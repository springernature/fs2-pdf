package fs2
package pdf

sealed trait ImageError

object ImageError
{
  case class Encode(message: String)
  extends ImageError

  case class Sanitize(message: String)
  extends ImageError

  case class Stream(message: String)
  extends ImageError

  case object NoImages
  extends ImageError
}
