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

The coarsest useful data types are provided by `wm.pdf.PdfStream.topLevel`, producing the ADT `TopLevel`:

```scala
case class IndirectObj(obj: pdf.IndirectObj)
case class Version(version: pdf.Version)
case class Comment(data: pdf.Comment)
case class Xref(version: pdf.Xref)
case class StartXref(startxref: pdf.StartXref)
```

PDFs consist of a series of objects that look like this:

```pdf
% a compressed content stream object
3 0 obj
<</Filter /FlateDecode /Length 19>>
stream
<binary data...>
endstream
endobj

% an array object
112 0 obj
[/Name 5 4 0 R (string)]
endobj

% a dict object
2624 0 obj
<</Count 48 /Kids [2625 0 R 2626 0 R 2627 0 R 2628 0 R 2629 0 R 2630 0 R] /Parent 2623 0 R /Type /Pages>>
endobj
```

These are encoded as `IndirectObj`, with the stream being optional.
There are special object streams, in which a stream contains more objects; those are decoded in a later stage.

At the very beginning of a document, the version header should appear:

```pdf
%PDF-1.7
%Ã¢Ã£ÃÃ

```

The second line is optional and indicates that the PDF contains binary data streams.

At the end of a document, the cross reference table, or xref, indicates the byte offsets of the contained objects,
looking like this:

```pdf
xref
0 1724
0000000000 65535 f 
0000111287 00000 n 
0000111518 00000 n 
0000111722 00000 n 
0000111822 00000 n 
0000112053 00000 n 
...
0000111175 00000 n 
trailer
<<
/ID [<9154668ac56ee69570067970a0db0b0a> <3ddbb5faba07f5306b8feb50afd4225c> ]
/Root 1685 0 R
/Size 1724
/Info 1683 0 R
>>
startxref
1493726
%%EOF

```

The dictionary after the `trailer` keyword contains metadata, in particular the `/Root` reference pointing to the object
describing the pages.

The number after the `startxref` keyword denotes the byte offset of the `xref` keyword for quicker seeking in viewer apps.

Multiple xrefs may occur in a document under two conditions:
* linearized PDFs, where an additional xref at the beginning of the document references only the first page, for
  optimized loading
* incrementally updated PDFs, allowing authoring tools to append arbitrarily many additional objects with xrefs

Xrefs can be compressed into binary streams.
In that case, only the part starting with `startxref` will be at the end of the file, and the `StartXref` variant will
encode this part.

In order to use this initial encoding, pipe a stream of bytes through `PdfStream.topLevel`:

```scala
val raw: Stream[IO, Byte] =
  openAPdfFile

val topLevel: Stream[IO, TopLevel] =
  raw.through(PdfStream.topLevel)
```

For a slightly more abstract encoding, `TopLevel` can be transformed into `Decoded` with the variants:

```scala
case class DataObj(obj: Obj)
case class ContentObj(obj: Obj, rawStream: BitVector, stream: Uncompressed)
case class Meta(xrefs: NonEmptyList[Xref], trailer: Trailer, version: Version)
```

Here, the distinction between objects with and without streams is made and the metadata is aggregated into a unique
record containing all xrefs, the aggregated trailer and the version.

To use this:

```scala
val decoded: Stream[IO, Decoded] =
  raw.through(PdfStream.decode(Log.noop)) // you can provide a real logger with `fs2.pdf.Log.io`
```

### Encoding

A stream of indirect objects can be encoded into a pdf document, with automatic generation of the cross reference table.

```scala
val reencoded: Stream[IO, Byte] =
  decoded
    .through(Decode.indirectObjs)
    .through(WritePdf.objects(Trailer(Prim.Dict.empty)))
```

### Validation

```scala
val result: IO[ValidatedNel[String, Unit]] = raw.through(PdfStream.validate(Log.noop))
```

# Development

### Testing

```bash
ops/sbt test
```

[fs2]: https://fs2.io
[scodec]: https://scodec.org
