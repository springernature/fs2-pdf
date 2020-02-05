package fs2
package pdf

import java.awt.image.BufferedImage

import cats.data.EitherT
import cats.effect.IO
import javax.imageio.ImageIO
import scodec.{Attempt, Decoder}
import scodec.bits.{BitVector, ByteOrdering, ByteVector}

case class Tiff(width: Long, height: Long, group: Long, blackIsZero: Long)

object Tiff
{
  val le = ByteOrdering.LittleEndian

  def short(a: Short): ByteVector =
    ByteVector.fromShort(a, size = 2, ordering = le)

  def long(a: Long): ByteVector =
    ByteVector.fromLong(a, size = 4, ordering = le)

  def ssll(a1: Short, a2: Short, a3: Long, a4: Long): ByteVector =
    short(a1) ++ short(a2) ++ long(a3) ++ long(a4)

  val headerSize: Int =
    108

  def header(params: Tiff, size: Long): ByteVector =
    ByteVector.fromByte('I') ++
    ByteVector.fromByte('I') ++
    short(42) ++
    long(8) ++
    short(8) ++
    ssll(256, 4, 1, params.width) ++
    ssll(257, 4, 1, params.height) ++
    ssll(258, 3, 1, 1) ++
    ssll(259, 3, 1, params.group) ++
    ssll(262, 3, 1, params.blackIsZero) ++
    ssll(273, 4, 1, headerSize) ++
    ssll(278, 4, 1, params.height) ++
    ssll(279, 4, 1, size) ++
    short(0)

  def fromData(params: Tiff)(data: ByteVector): ByteVector =
    header(params, data.size) ++ data

  def image(params: Tiff)(data: ByteVector): EitherT[IO, String, BufferedImage] =
    EitherT(IO(ImageIO.read(JavaStream.bais(fromData(params)(data).toArray))).attempt)
      .leftMap(_.getMessage)

  def decoder: Decoder[Unit] = {
    import scodec.codecs._
    val s = shortL(16)
    val l = longL(32)
    val entry = s.withContext("entry 1") ~ s.withContext("entry 2") ~ l.withContext("entry 3") ~ l.withContext("entry 4")
    constant(BitVector.fromByte('I')) ~>
    constant(BitVector.fromByte('I')) ~>
    constant(BitVector.fromShort(42, 16, le)).withContext("version") ~>
    constant(BitVector.fromLong(8, 32, le)).withContext("offset") ~> (
      for {
        _ <- listOfN(s.map(_.toInt).decodeOnly, entry)
        _ <- constant(BitVector.fromShort(0))
      } yield ()
    ).decodeOnly
  }

  def raw(data: BitVector): Attempt[BitVector] =
    decoder.decode(data).map(_.remainder)
}
