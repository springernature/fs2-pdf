package fs2
package pdf

import scala.util.Try

import cats.implicits._
import codec.{Codecs, Many, Text, Whitespace}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err}
import scodec.bits.{BitVector, ByteVector}
import scodec.codecs.liftF2ToNestedTupleF
import scodec.interop.cats.AttemptMonadErrorInstance

/**
  * Algebra for PDF primitives.
  *
  * Each object contains one primitive, most frequently a [[Dict]].
  */
sealed trait Prim

object Prim
extends PrimCodec
{
  /**
    * The literal string 'null'.
    */
  case object Null
  extends Prim

  /**
   * A reference to another object, encoded as 'number generation R' (like '5 0 R')
    *
    * @param number the unique object identifier
    * @param generation the indicator for replacements for deleted objects
    */
  case class Ref(number: Long, generation: Int)
  extends Prim

  /**
    * A regular boolean, either of the literal strings 'true' and 'false'.
    */
  case class Bool(value: Boolean)
  extends Prim

  /**
    * A number, either floating point or integer.
    */
  case class Number(data: BigDecimal)
  extends Prim

  /**
    * A name is encoded with a leading solidus '/', like '/Type'.
    * It is used for dictionary keys and identifiers.
    */
  case class Name(data: String)
  extends Prim

  /**
    * A string is encoded literally and wrapped with parentheses, like '(a string)'.
    */
  case class Str(data: ByteVector)
  extends Prim

  /**
    * A hexadecimal string is wrapped with angular brackets, like '<4e8a1db>'.
    */
  case class HexStr(data: ByteVector)
  extends Prim

  /**
    * An array is enclosed in square brackets and can contain a heterogeneous combination of other primitives,
    * separated by spaces, like '[(hello) 1 0 R << /Key /Value >> 5.4 null false]'.
    */
  case class Array(data: List[Prim])
  extends Prim

  object Array
  {
    def refs(nums: Long*): Array =
      Array(nums.map(refNum).toList)

    def nums(nums: BigDecimal*): Array =
      Array(nums.map(Number(_)).toList)
  }

  case class Dict(data: Map[String, Prim])
  extends Prim
  {
    def apply(key: String): Option[Prim] =
      data.lift(key)

    def ++(other: Dict): Dict =
      Dict(data ++ other.data)

    def ref(key: String): Option[Ref] =
      apply(key).collect { case r @ Ref(_, _) => r }
  }

  object Dict
  {
    def empty: Dict =
      Dict(Map.empty)

    def attempt(key: String)(data: Prim): Attempt[Prim] =
      Attempt.fromOption(tryDict(key)(data), Err(s"key `$key` not present in $data"))

    def path[A](keys: String*)(data: Prim)(extract: PartialFunction[Prim, A]): Attempt[A] = {
      def p = keys.mkString("->")
      keys
        .toList
        .foldLeftM[Attempt, Prim](data)((d, key) => attempt(key)(d))
        .flatMap(a => Attempt.fromOption(extract.lift(a), Err(s"extraction failed for `$p`")))
    }

    def name(keys: String*)(data: Prim): Attempt[String] =
      path(keys: _*)(data) { case Name(data) => data }

    def number(keys: String*)(data: Prim): Attempt[BigDecimal] =
      path(keys: _*)(data) { case Number(data) => data }

    def array(keys: String*)(data: Prim): Attempt[List[Prim]] =
      path(keys: _*)(data) { case Array(data) => data }

    def numbers(keys: String*)(data: Prim): Attempt[List[BigDecimal]] =
      array(keys: _*)(data).flatMap(_.traverse {
        case Number(data) => Attempt.successful(data)
        case a => Scodec.fail(s"wrong type: $a")
      })

    def collectRefs(keys: String*)(data: Prim): Attempt[List[Long]] =
      array(keys: _*)(data).map(_.collect { case Ref(number, _) => number })

    def appendOrCreateArray(key: String)(elem: Prim)(dict: Dict): Dict =
      Prim.Dict(dict.data.updatedWith(key)(old => Some(Prim.appendOrCreateArray(elem)(old))))

    def appendOrCreateDict(key: String)(elemKey: String, elem: Prim)(dict: Dict): Dict =
      Prim.Dict(dict.data.updatedWith(key)(old => Some(Prim.appendOrCreateDict(elemKey, elem)(old))))

    def updated(key: String, value: Prim)(dict: Dict): Dict =
      Dict(dict.data.updated(key, value))

    def deepMergeValues(key: String): (Prim, Prim) => Attempt[Prim] = {
      case (update @ Dict(_), original @ Dict(_)) =>
        deepMerge(update)(original)
      case (update, original @ Dict(_)) =>
        Scodec.fail(s"incompatible types for Prim.Dict.deepMerge: $key | $update | $original")
      case (Array(update), Array(original)) =>
        Attempt.successful(Array(original ++ update))
      case (update, Array(original)) =>
        Attempt.successful(Array(original :+ update))
      case (update, original) =>
        Scodec.fail(s"incompatible types for Prim.Dict.deepMerge: $key | $update | $original")
    }

    def deepMerge(update: Dict)(original: Dict): Attempt[Dict] =
      update.data.toList.foldLeftM(original) {
        case (z, (key, updateValue)) =>
          z.data.lift(key) match {
            case Some(originalValue) =>
              deepMergeValues(key)(updateValue, originalValue)
                .map(updated(key, _)(z))
            case None =>
              Attempt.successful(updated(key, updateValue)(z))
          }
      }
  }

  def appendOrCreateArray(elem: Prim): Option[Prim] => Prim = {
    case Some(Prim.Array(old)) => Prim.Array(old :+ elem)
    case Some(old @ Prim.Ref(_, _)) => Prim.Array(List(old, elem))
    case _ => Prim.Array(List(elem))
  }

  def appendOrCreateDict(key: String, elem: Prim): Option[Prim] => Prim = {
    case Some(Prim.Dict(old)) => Prim.Dict(old + ((key, elem)))
    case _ => Prim.Dict(Map(key -> elem))
  }

  def withDict[A]: Prim => A => Option[(A, Prim.Dict)] = {
    case data @ Prim.Dict(_) => a => Some((a, data))
    case _ => _ => None
  }

  def liftWithKey(key: String)(data: Prim): Option[(String, Prim.Dict)] =
    tryDict(key)(data)
      .collect { case Name(tpe) => tpe }
      .flatMap(withDict(data))

  object tpe
  {
    def unapply(data: Prim): Option[(String, Prim.Dict)] =
      liftWithKey("Type")(data)
  }

  object subtype
  {
    def unapply(data: Prim): Option[(String, Prim.Dict)] =
      liftWithKey("Subtype")(data)
  }

  object filter
  {
    def unapply(data: Prim): Option[(String, Prim.Dict)] =
      liftWithKey("Filter")(data)
  }

  object contents
  {
    def unapply(data: Prim): Option[Prim] =
      tryDict("Contents")(data)
  }

  object fonts
  {
    def inRoot(data: Prim): Option[List[Long]] =
      tryDict("Font")(data)
        .flatMap {
          case Ref(num, 0) => Some(List(num))
          case Dict(data) => Some(data.toList.collect { case (_, Ref(num, 0)) => num })
          case _ => None
        }

    def inResources(data: Prim): Option[List[Long]] =
      tryDict("Resources")(data)
        .flatMap(inRoot)

    def unapply(data: Prim): Option[List[Long]] =
      inRoot(data).orElse(inResources(data))
  }

  object fontResources
  {
    def unapply(prim: Prim): Option[Dict] =
      prim match {
        case dict @ Dict(data) if data.contains("Font") => Some(dict)
        case _ => None
      }
  }

  object linearization
  {
    def unapply(data: Prim): Boolean =
      tryDict("Linearized")(data).isDefined
  }

  object refs {
    def unapply(data: Prim): Option[List[Ref]] =
      data match {
        case Prim.Array(elems) =>
          elems.collect {
            case r@Ref(_, _) => Some(r)
            case _ => None
          }.sequence
        case _ =>
          None
      }
  }

  def path[A](keys: String*)(data: Prim)(extract: PartialFunction[Prim, A]): Option[A] = {
    keys.toList.foldLeftM[Option, Prim](data) {
      (d, key) =>
        tryDict(key)(d)
    }
      .flatMap(extract.lift)
  }

  def dictOnly: Prim => Option[Map[String, Prim]] = {
    case Dict(data) => Some(data)
    case _ => None
  }

  def containsName(key: String)(name: String)(data: Prim.Dict): Boolean = {
    data.data.lift(key) match {
      case Some(Prim.Name(actual)) =>
        actual == name
      case Some(Prim.Array(elems)) =>
        elems.exists {
          case Prim.Name(actual) => actual == name
          case _ => false
        }
      case _ =>
        false
    }
  }

  def dict(values: (String, Prim)*): Prim.Dict =
    Dict(values.toMap)

  def array(values: Prim*): Prim =
    Array(values.toList)

  def ref(to: Obj): Prim =
    Prim.Ref(to.index.number, to.index.generation)

  def refI(to: Obj.Index): Prim =
    Prim.Ref(to.number, to.generation)

  def refT(to: IndirectObj): Prim =
    refI(to.obj.index)

  def refNum(to: Long): Prim =
    Prim.Ref(to, 0)

  def num(num: BigDecimal): Prim =
    Prim.Number(num)

  def str(s: String): Prim.Str =
    Prim.Str(ByteVector(s.getBytes))

  def hexStr(s: String): Prim.HexStr =
    Prim.HexStr(ByteVector(s.getBytes))

  def tryDict(key: String): Prim => Option[Prim] = {
    case Dict(data) => data.lift(key)
    case _ => None
  }

  def isType(tpe: String)(data: Map[String, Prim]): Boolean =
    data.lift("Type").contains(Prim.Name(tpe))
}

private[pdf]
trait PrimCodec
{
  import scodec.codecs.{optional, recover, lazily, bytes}
  import Prim.{str => _, _}
  import Whitespace.{space, ws, skipWs, nlWs}
  import Codecs.{opt, bracketChar}
  import Text.{str, ascii, char, ranges, charsNoneOf}

  def Codec_Null: Codec[Null.type] =
    str("null").xmap(_ => Null, _ => ())

  def Codec_Ref: Codec[Ref] =
    ((ascii.long <~ space) ~ ascii.int <~ space <~ char('R'))
      .xmap(Prim.Ref(_, _), { case Prim.Ref(n, g) => (n, g) })

  def encodeBool: Bool => Attempt[BitVector] = {
    case Bool(true) => Attempt.successful(BitVector("true".getBytes))
    case Bool(false) => Attempt.successful(BitVector("false".getBytes))
  }

  def decodeBool: Decoder[Bool] =
    Decoder.choiceDecoder(
      str("true").map(_ => Bool(true)),
      str("false").map(_ => Bool(false)),
    )

  def Codec_Bool: Codec[Bool] =
    Codec(Encoder(encodeBool), decodeBool)

  def formatNumber: (((Option[Unit], String), Option[String])) => String = {
    case ((minus, pre), post) =>
      val frac = post.fold("")(a => s".$a")
      val m = minus.fold("")(_ => "-")
      s"$m$pre$frac"
  }

  def splitNumber(num: Prim.Number): Attempt[((Option[Unit], String), Option[String])] =
    num.data.toString.split(raw"\.").toList match {
      case List(pre, post) => Attempt.successful(((None, pre), Some(post)))
      case List(n) => Attempt.successful(((None, n), None))
      case a => Scodec.fail(s"invalid number: $num ($a)")
    }

  def Codec_Number: Codec[Number] =
    (
      opt(char('-')) ~
      ascii.digits1 ~
      optional(recover(char('.')), ascii.digits1)
    )
      .exmap(
        a => Attempt.fromTry(Try(BigDecimal(formatNumber(a)))).map(Prim.Number(_)),
        splitNumber,
      )

  def findClosingParen: Decoder[ByteVector] =
    Decoder {
      bits =>
        def rec(parens: Int)(input: ByteVector, output: ByteVector)
        : Attempt[(ByteVector, ByteVector)] = {
          input.headOption match {
            case Some('\\') =>
              rec(parens)(input.drop(2), output ++ input.take(2))
            case Some(c @ '(') =>
              rec(parens + 1)(input.drop(1), output :+ c)
            case Some(c @ ')') if parens > 0 =>
              rec(parens - 1)(input.drop(1), output :+ c)
            case Some(c @ ')') =>
              Attempt.successful((output, input))
            case Some(c) =>
              rec(parens)(input.drop(1), output :+ c)
            case None =>
              Scodec.fail("could not find closing parenthesis for Str")
          }
        }
        rec(0)(bits.bytes, ByteVector.empty)
          .map { case (result, remainder) => DecodeResult(result, remainder.bits) }
    }

  def strBytes: Codec[ByteVector] =
    (char('(') ~> Codec(bytes, findClosingParen) <~ char(')'))

  def Codec_Str: Codec[Str] =
    strBytes.as

  def hexChar: Codec[ByteVector] =
    ranges(('a', 'f'), ('A', 'F'), ('0', '9'))

  def Codec_HexStr: Codec[HexStr] =
    bracketChar('<', '>')(hexChar).as

  def trim[A](inner: Codec[A]): Codec[A] =
    skipWs ~> inner <~ skipWs

  def Codec_Array: Codec[Array] =
    (skipWs ~> Many.bracket(trim(char('[')), trim(char(']')))(lazily(trim(Codec_Prim)) <~ ws))
      .as[Array]
      .withContext("array")

  def nameString: Codec[String] =
    trim(
      char('/').withContext("name solidus") ~>
      charsNoneOf(Text.latin)("/<>[]( \r\n".toList).withContext("name chars")
    )
      .withContext("name string")

  def dictElem: Codec[(String, Prim)] =
    (nlWs ~> nameString.withContext("dict key") ~ (ws ~> trim(lazily(Codec_Prim)).withContext("dict value")))
      .withContext("dict elem")

  def dictStartMarker: Codec[Unit] =
    trim(str("<<"))
      .withContext("dict<<")

  def dictEndMarker: Codec[Unit] =
    (nlWs ~> trim(str(">>")))
      .withContext("dict>>")

  def Codec_Dict: Codec[Dict] =
    Many.bracket(dictStartMarker, dictEndMarker)(dictElem)
      .xmap[Map[String, Prim]](Map.from, _.toList)
      .as[Dict]
      .withContext("dict")

  def Codec_Name: Codec[Name] =
    nameString
      .as[Name]
      .withContext("name")

  /**
    * Constructed manually rather than with [[Codec.coproduct]] to force the order of codecs, since a Ref will decode
    * successfully with Codec_Number
    */
  implicit def Codec_Prim: Codec[Prim] =
    (
      Codec_Name :+:
      Codec_Null :+:
      Codec_Bool :+:
      Codec_Ref :+:
      Codec_Number :+:
      Codec_Str :+:
      Codec_HexStr :+:
      Codec_Array :+:
      Codec_Dict
    )
      .as[Prim]
      .choice
}
