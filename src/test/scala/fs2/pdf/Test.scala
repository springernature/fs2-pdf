package fs2
package pdf

import cats.effect.IO
import org.specs2.matcher.MatchResult
import org.specs2.mutable.SpecificationLike

object Test
extends SpecificationLike
{
  def unitStream(s: Stream[IO, Unit]): MatchResult[Any] =
    s.compile.drain.unsafeRunSync.must_==(())
}
