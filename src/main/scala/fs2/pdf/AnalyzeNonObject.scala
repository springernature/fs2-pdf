package fs2
package pdf

import scodec.{Attempt, Decoder, Err}
import scodec.bits.ByteVector

object AnalyzeNonObject
{
  def withoutComments[A](inner: Decoder[A]): Decoder[A] =
    Decoder(bits => inner.decode(Codecs.removeCommentsBits(bits)))

  def decoder: Decoder[Analyzed] =
    Decoder.choiceDecoder(
      withoutComments(Xref.startxref).map(Analyzed.StartXref(_)),
      withoutComments(Xref.Codec_Xref).map(Analyzed.Xref(_)),
      Version.Codec_Version.map(Analyzed.Version(_)),
    )
      .decodeOnly
      .withContext("non-object data")

  def apply(data: ByteVector): Attempt[List[Analyzed]] =
    decoder.decode(data.bits)
      .map(a => List(a.value))
      .mapErr { case e => Err(s"parsing raw: ${e.messageWithContext}\n${Codecs.sanitize(data)}\n") }
}
