package fs2
package pdf

import scodec.{Attempt, DecodeResult, Decoder, Err}
import scodec.bits.ByteVector

object AnalyzeNonObject
{
  def decoder: Decoder[Analyzed] =
    Decoder.choiceDecoder(
      Xref.startxref.map(Analyzed.StartXref(_)),
      Xref.Codec_Xref.map(Analyzed.Xref(_)),
      Version.Codec_Version.map(Analyzed.Version(_)),
    )
      .decodeOnly
      .withContext("non-object data")

  def apply(data: ByteVector): Attempt[List[Analyzed]] =
    Version.Codec_Version.decode(data.bits) match {
      case Attempt.Failure(_) =>
        decoder.decode(Codecs.removeComments(data).bits)
          .map(a => List(a.value))
          .mapErr { case e => Err(s"parsing raw: ${e.messageWithContext}\n${Codecs.sanitize(data)}\n") }
      case Attempt.Successful(DecodeResult(result, _)) =>
        Attempt.successful(List(Analyzed.Version(result)))
    }
}
