package fs2
package pdf

import cats.effect.IO
import fs2.{Pipe, Pull, Stream}
import scodec.bits.ByteVector

case class RewriteState[S](state: S, trailer: Option[Trailer], root: Option[Prim.Ref])

object RewriteState
{
  def cons[S](state: S): RewriteState[S] =
    RewriteState(state, None, None)
}

case class RewriteUpdate[S](state: S, trailer: Trailer)

/**
  * A utility for transforming a stream of indirect objects into [[Part]]s encodable with [[WritePdf]] by calling a
  * stateful function for each element and finalizing with another function consuming the collected state.
  */
object Rewrite
{
  private[pdf]
  def emitUpdate[S]: RewriteState[S] => Pull[IO, Part[Trailer], RewriteUpdate[S]] = {
    case RewriteState(state, Some(trailer), _) =>
      Pull.output1(Part.Meta(trailer))
        .as(RewriteUpdate(state, trailer))
    case RewriteState(state, None, Some(root)) =>
      val trailer = Trailer(-1, Prim.dict("Root" -> root), Some(root))
      Pull.output1(Part.Meta(trailer)).as(RewriteUpdate(state, trailer))
    case RewriteState(_, None, None) =>
      StreamUtil.failPull("no trailer or root in rewrite stream")
  }

  private[pdf]
  def rewrite[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => Pull[IO, Part[Trailer], RewriteState[S]])
  (in: Stream[IO, A])
  : Pull[IO, Part[Trailer], RewriteUpdate[S]] =
    StreamUtil.pullState(collect)(in)(RewriteState.cons(initial))
      .flatMap(emitUpdate)

  private[pdf]
  def rewriteAndUpdate[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => Pull[IO, Part[Trailer], RewriteState[S]])
  (update: RewriteUpdate[S] => Pull[IO, Part[Trailer], Unit])
  (in: Stream[IO, A])
  : Pull[IO, Part[Trailer], Unit] =
    rewrite(initial)(collect)(in)
      .flatMap(update)

  def apply[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => Pull[IO, Part[Trailer], RewriteState[S]])
  (update: RewriteUpdate[S] => Pull[IO, Part[Trailer], Unit])
  : Pipe[IO, A, ByteVector] =
    _
      .through(rewriteAndUpdate(initial)(collect)(update)(_).stream)
      .through(WritePdf.parts)

  private[pdf]
  def simpleCollect[S, A]
  (collect: RewriteState[S] => A => (List[Part[Trailer]], RewriteState[S]))
  (state: RewriteState[S])
  (a: A)
  : Pull[IO, Part[Trailer], RewriteState[S]] = {
    val (part, newState) = collect(state)(a)
    Pull.output(Chunk.seq(part)).as(newState)
  }

  def simpleParts[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => (List[Part[Trailer]], RewriteState[S]))
  (update: RewriteUpdate[S] => Part[Trailer])
  : Pipe[IO, A, Part[Trailer]] =
    rewriteAndUpdate(initial)(simpleCollect(collect))(update.andThen(Pull.output1))(_).stream

  def simple[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => (List[Part[Trailer]], RewriteState[S]))
  (update: RewriteUpdate[S] => Part[Trailer])
  : Pipe[IO, A, ByteVector] =
    apply(initial)(simpleCollect(collect))(update.andThen(Pull.output1))

  def forState[S, A]
  (initial: S)
  (collect: RewriteState[S] => A => Pull[IO, Part[Trailer], RewriteState[S]])
  : Pipe[IO, A, S] =
    in => {
      val p = for {
        result <- rewrite(initial)(collect)(in).mapOutput(Left(_))
        _ <- Pull.output1(Right(result))
      } yield ()
      p.stream.collect { case Right(RewriteUpdate(s, _)) => s }
    }

  def noUpdate[S]: RewriteUpdate[S] => Part[Trailer] =
    update => Part.Meta(update.trailer)
}
