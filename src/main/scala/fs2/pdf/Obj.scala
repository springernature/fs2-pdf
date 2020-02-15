package fs2
package pdf

import scodec.Codec
import scodec.codecs.liftF2ToNestedTupleF

case class Obj(index: Obj.Index, data: Prim)

object Obj
{
  import Whitespace.{nlWs, skipWs, ws}
  import Codecs.{ascii, str, productCodec}

  case class Index(number: Long, generation: Int)

  object Index
  {
    def indexRaw: Index => String = {
      case Index(number, generation) =>
        s"$number $generation obj\n"
    }

    implicit def Codec_Index: Codec[Index] =
      ((ascii.long <~ str(" ")) ~ (ascii.int <~ str(" obj")))
        .xmap(Obj.Index(_, _), a => (a.number, a.generation))
  }

  def withDict[A]: Obj => A => Option[(A, Prim.Dict)] = {
    case Obj(_, data @ Prim.Dict(_)) => a => Some((a, data))
    case _ => _ => None
  }

  object tpe
  {
    def unapply(obj: Obj): Option[(String, Prim.Dict)] =
      Prim.tpe.unapply(obj.data)
  }

  object subtype
  {
    def unapply(obj: Obj): Option[(String, Prim.Dict)] =
      Prim.subtype.unapply(obj.data)
  }

  implicit def Codec_Obj: Codec[Obj] =
    productCodec(
      (skipWs ~> Index.Codec_Index <~ nlWs) ~
      Prim.Codec_Prim <~ (nlWs <~ str("endobj") <~ nlWs)
    )

  def codecPreStream: Codec[Obj] =
    productCodec(
      (skipWs ~> Index.Codec_Index <~ ws) ~
      Prim.Codec_Prim <~ ws
    )
}
