package fs2
package pdf
package codec

import scodec.{Codec, Decoder}
import scodec.bits.{BitVector, ByteVector, HexStringSyntax}
import scodec.codecs._

private[pdf]
object Newline
{
  val lfBytes: ByteVector =
    hex"0A"

  val lfBits: BitVector =
    lfBytes.bits

  val lfByte: Byte =
    lfBytes.toByte()

  val crBytes: ByteVector =
    hex"0D"

  val crBits: BitVector =
    crBytes.bits

  val crByte: Byte =
    crBytes.toByte()

  val crlfBytes: ByteVector =
    hex"0D0A"

  val crlfBits: BitVector =
    crlfBytes.bits

  val spaceBytes: ByteVector =
    hex"20"

  val spaceByte: Byte =
    spaceBytes.toByte()

  val tabBytes: ByteVector =
    hex"0B"

  val newlines: List[ByteVector] =
    List(lfBytes, crlfBytes, crBytes)

  val lf: Codec[Unit] =
    constant(lfBytes)

  val cr: Codec[Unit] =
    constant(crBytes)

  val crlf: Codec[Unit] =
    constant(crlfBytes)

  val newline: Codec[Unit] =
    choice(constant(lfBytes), constant(crlfBytes), constant(crBytes))

  val newlineBytes: Decoder[ByteVector] =
    Decoder.choiceDecoder(
      constant(lfBytes).map(_ => lfBytes),
      constant(crlfBytes).map(_ => crlfBytes),
      constant(crBytes).map(_ => crBytes),
    )

  val newlineBits: Decoder[BitVector] =
    Decoder.choiceDecoder(
      constant(lfBytes).map(_ => lfBytes.bits),
      constant(crlfBytes).map(_ => crlfBytes.bits),
      constant(crBytes).map(_ => crBytes.bits),
    )

  def stripNewline(bytes: ByteVector): ByteVector =
    if (bytes.takeRight(2) == crlfBytes) bytes.dropRight(2)
    else if (bytes.takeRight(1) == lfBytes) bytes.dropRight(1)
    else if (bytes.takeRight(1) == crBytes) bytes.dropRight(1)
    else bytes

  def stripNewlineBits(bits: BitVector): BitVector =
    stripNewline(bits.bytes).bits

  def isNewlineByte(b: Byte): Boolean =
    b == lfByte || b == crByte
}
