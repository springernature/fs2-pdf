package fs2
package pdf

import org.specs2.mutable.Specification
import scodec.bits.ByteVector

class RemoveCommentTest
extends Specification
{
  val source = """1%comment
  |%%EOF
  |3
  |4%com ment 
  |%commmment %% asdf % foo
  |""".stripMargin

  val target =
    """1
    |%%EOF
    |3
    |4
    |
    |""".stripMargin

  "remove comments" >>
  Codecs.removeComments(ByteVector(source.getBytes)).decodeUtf8.must(beRight(target))
}
