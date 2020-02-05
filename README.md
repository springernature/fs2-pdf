# About

**fs2-pdf** is a Scala library for manipulating PDF files in [fs2] streams using [scodec] for parsing.

Prevalent PDF manipulation tools like iText require the whole file to be read into memory, making it hard to estimate
the memory footprint due to large images and imposing a hard boundary for the document file size of `Int.MaxValue`.

## Module ID

```sbt
"com.springernature" %% "fs2-pdf" % "0.1.0"
```

# Usage

The provided `fs2` pipes convert pdf data in the shape of a `Stream[IO, Byte]` into data type encodings and back to byte
streams.
Raw data is processed with [scodec] and stored as `BitVector`s and `ByteVector`s.

```scala
import fs2.pdf._
```

### Decoding

The coarsest useful data types are provided by `wm.pdf.StreamParser.objects`, producing the ADT `Parsed`:

```scala
case class IndirectObj(obj: Obj, stream: Option[Parsed.Stream], original: ByteVector)
case class StreamObject(obj: Obj)
case class Unparsable(index: Obj.Index, data: ByteVector)
case class Raw(bytes: ByteVector)
```

PDFs consist of a series of objects that look like this:

```pdf
3 0 obj
<</Filter /FlateDecode /Length 19>>
stream
<binary data...>
endstream
endobj

2624 0 obj
<</Count 48 /Kids [2625 0 R 2626 0 R 2627 0 R 2628 0 R 2629 0 R 2630 0 R] /Parent 2623 0 R /Type /Pages>>
endobj
```

These are encoded as `IndirectObj`, with the stream being optional.
There are special object streams, in which a stream contains more objects; those are encoded as `StreamObject`.
If parsing of the data in an object failed, but the index numbers have been read successfully, `Unparsable` is used.
For any other data, including the cross reference table and version header, `Raw` is emitted.

```scala
val raw: Stream[IO, Byte] =
  openAPdfFile

val parsed: Stream[IO, Parsed] =
  raw.through(StreamParser.objects(Log.noop)) // you can provide a real logger with `fs2.watermark.Log.io`
```

### Encoding

A stream of indirect objects can be encoded into a pdf document, with automatic generation of the cross reference table.

```scala
val reencoded: Stream[IO, Byte] =
  parsed
    .through(Parsed.indirectObjs)
    .through(WritePdf.objects(Trailer(Prim.Dict.empty)))
```

### Validation

```scala
val result: IO[ValidatedNel[String, Unit]] = raw.through(StreamParser.validate(Log.noop))
```

# Development

### Testing

```bash
ops/sbt test
```

[fs2]: https://fs2.io
[scodec]: https://scodec.org
