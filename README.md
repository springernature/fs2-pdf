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

## Decoding

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

For another level of abstraction, the `Element` algebra represents semantics of objects:

```scala
object DataKind
{
  case object General
  case class Page(page: pdf.Page)
  case class Pages(pages: pdf.Pages)
  case class Array(data: Prim.Array)
  case class FontResource(res: pdf.FontResource)
}

case class Data(obj: Obj, kind: DataKind)

object ContentKind
{
  case object General
  case class Image(image: pdf.Image)
}

case class Content(obj: Obj, rawStream: BitVector, stream: Uncompressed, kind: ContentKind)

case class Meta(trailer: Trailer, version: Version)
```

To use this:

```scala
val elements: Stream[IO, Element] =
  raw.through(PdfStream.elements(Log.noop))
```

## Encoding

A stream of indirect objects can be encoded into a pdf document, with automatic generation of the cross reference table.

```scala
val reencoded: Stream[IO, Byte] =
  decoded
    .through(Decoded.parts)
    .through(WritePdf.parts)
    .through(Write.bytes("/path/to/file.pdf"))
```

or

```scala
val reencoded: Stream[IO, Byte] =
  elements
    .through(Element.parts)
    .through(WritePdf.parts)
    .through(Write.bytes("/path/to/file.pdf"))
```

The intermediate data type `Part` is used to carry over the trailer into the encoder.
Instead of `Decoded.parts` and `WritePdf.parts`, you could also use `Decoded.objects` and `WritePdf.objects`, but then you would
have to specify a trailer dictionary for the encoder.
`Decoded.parts` extracts this information from the input trailer.

## Transforming

Since you won't just want to reencode the original data, a step in between decoding and encoding should manipulate it.
The pipes in `Rewrite` are used in `Decoded.parts`, and they allow more complex transformations by keeping a state when
analyzing objects and using it to create additions to the document.

The rewrite works in two stages, `collect` and `update`:

```scala
case class PagesState(pages: List[Pages])
val initialState: PagesState = PagesState(Nil)
val result: Stream[IO, ByteVector] =
  elements
    .through(Rewrite(initialState)(collect)(update))
```

In `collect`, every `Element` (or `Decoded`) will be evaluated by a stateful function that allows you to prevent objects from
being written and instead collect them for an update:

```scala
def collect(state: RewriteState[PagesState]): Element => Pull[IO, Part[Trailer], RewriteState[PagesState]] = {
  case Element.Data(_, Element.DataKind.Pages(pages)) =>
    Pull.pure(state.copy(state = state.state.copy(pages = pages :: state.state.pages)))
  case Element.obj(obj) =>
    Pull.output1(Part.Obj(obj)) >> Pull.pure(state)
  case Element.Meta(trailer, _) =>
    Pull.pure(state.copy(trailer = Some(trailer)))
}
```

Here we get our state, wrapped in `RewriteState`, which tracks the trailer, and an `Element` passed into our function.
The output is `fs2.Pull`, on which you can call `Pull.output1` to instruct the stream to emit a PDF part, and `Pull.pure` to
return the updated state.

There is a variant `Rewrite.simple` that hides the `Pull` from these signatures and instead expects you to return
`(List[Part[Trailer]], RewriteState[PagesState])`.

In this example, we match on `DataKind.Pages` and do not call `Pull.output1` in this case, but add the pages to our state.
In the case of any other object, which we match with the convenience extractor `Element.obj`, we just pass through as a
`Part.Obj`.
Finally, the trailer has to be carried over in the state.

In `update`, we use the collected pages to do some analysis and then write them back to the stream:

```scala
val fontObj(number: Long): IndirectObj =
  ???

def update(update: RewriteUpdate[PagesState]): Pull[IO, Part[Trailer], Unit] =
  Pull.output1(Part.Trailer(update.trailer)) >> update.state.pages.traverse_ {
    case Pages(index, data, _, true) =>
      val updatedData = data ++ Prim.dict("Resources" -> Prim.dict("Font" -> Prim.dict("F1" -> Prim.Ref(1000L, 0))))
      Pull.output1(fontObj(1000L)) >>
      Pull.output1(Part.Obj(IndirectObj(index, updatedData, None)))
    case Pages(index, data, _, false) =>
      Pull.output1(Part.Obj(IndirectObj(index, data, None)))
  }
```

`RewriteUpdate` is the same as `RewriteState`, except that the trailer isn't optional anymore.
If there is no trailer in the state at the end of the stream, an error is raised.

In this function, we first emit the trailer, then we iterate over our collected pages and match on the boolean `root`
field.
If we found the root page tree object, we first emit a custom font descriptor (not implemented here), then add
a reference to it to the page root's `Resource` dictionary (this is of course an incomplete simplification).
For all other pages objects, we just write the original data.

## Validation

```scala
val result: IO[ValidatedNel[String, Unit]] = raw.through(PdfStream.validate(Log.noop))
```

# Limitations

Linearization is not possible at the moment, since the linearization parameter dict is the first object and needs
information that is only available later, like the total file size.

A heuristical method for keeping already linearized documents intact is in development.

# Development

### Testing

```bash
ops/sbt test
```

[fs2]: https://fs2.io
[scodec]: https://scodec.org
