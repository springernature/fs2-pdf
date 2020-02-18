package fs2
package pdf

import codec.{Text, Whitespace}
import scodec.Codec
import scodec.codecs.liftF2ToNestedTupleF

/**
  * The data part of an indirect object or a stream object.
  *
  * @param index the object's number and generation number
  * @param data the object's data, a dict or other primitive
  */
case class Obj(index: Obj.Index, data: Prim)

object Obj
extends ObjCodec
{
  import Text.{ascii, str}
  import Whitespace.space

  /**
    * The indexing metadata of an object
    *
    * @param number a unique number used to reference the object from others and to index the xref table
    * @param generation indicates that the object replaces a previous, deleted object if nonzero
    */
  case class Index(number: Long, generation: Int)

  object Index
  {
    implicit def Codec_Index: Codec[Index] =
      ((ascii.long <~ space) ~ (ascii.int <~ space <~ str("obj")))
        .xmap(Obj.Index(_, _), a => (a.number, a.generation))
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

  object dict
  {
    def unapply(obj: Obj): Option[(Long, Prim.Dict)] =
      obj match {
        case Obj(Obj.Index(number, _), dict @ Prim.Dict(_)) =>
          Some(number, dict)
        case _ =>
          None
      }
  }
}

private[pdf]
trait ObjCodec
{
  import Whitespace.{nlWs, skipWs}
  import Text.str

  val codecPreStream: Codec[Obj] =
    ((skipWs ~> Codec[Obj.Index] <~ nlWs) :: Prim.Codec_Prim <~ nlWs)
      .as[Obj]

  implicit val Codec_Obj: Codec[Obj] =
    (codecPreStream <~ str("endobj") <~ nlWs)
      .as[Obj]
}
