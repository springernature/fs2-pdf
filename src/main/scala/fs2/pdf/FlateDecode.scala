package fs2
package pdf

import java.io.{InputStream, OutputStream}

import cats.effect.IO
import cats.implicits._
import scodec.{Attempt, DecodeResult, Err}
import scodec.bits.BitVector

object PredictorTransform
{
  def param(key: String, default: Int)(params: Prim): Int =
    Prim.Dict.number(key)(params).map(_.toInt).getOrElse(default)

  def unsafeDecode(params: Prim, predictor: BigDecimal)(is: InputStream, os: OutputStream): Unit =
    image.Predictor.decodePredictor(
      predictor.toInt,
      param("Colors", 1)(params),
      param("BitsPerComponent", 8)(params),
      param("Columns", 1)(params),
      is,
      os,
    )

  def decode(stream: BitVector, predictor: BigDecimal, params: Prim): IO[BitVector] =
    JavaStream.withByteStreams(stream.toByteArray)(
      (is, os) => IO(unsafeDecode(params, predictor)(is, os))
    ).map(BitVector(_))

  def apply(stream: BitVector, predictor: BigDecimal, params: Prim): Attempt[BitVector] =
    Attempt.fromEither(decode(stream, predictor, params).attempt.unsafeRunSync.leftMap(a => Err(a.toString)))
}

object FlateDecode
{
  import scodec.codecs._

  def handlePredictor(stream: BitVector, params: Prim.Dict): Option[Attempt[BitVector]] =
    Prim.path("Predictor")(params) {
      case Prim.Number(predictor) if predictor > 1 =>
        PredictorTransform(stream, predictor, params)
    }

  def handleParams(stream: BitVector, data: Prim): Attempt[BitVector] =
    Prim.path("DecodeParms")(data) { case params @ Prim.Dict(_) => handlePredictor(stream, params) }
      .flatten
      .getOrElse(Attempt.successful(stream))

  def apply(stream: BitVector, data: Prim): Attempt[BitVector] =
    for {
      DecodeResult(inflated, _) <- zlib(bits).decode(stream)
      result <- handleParams(inflated, data)
    } yield result
}
