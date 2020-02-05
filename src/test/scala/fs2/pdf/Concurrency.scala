package fs2
package pdf

import java.util.concurrent.{ExecutorService, Executors}

import scala.concurrent.ExecutionContext

import cats.effect.{Blocker, ContextShift, IO, Resource}
import fs2.Stream

object Concurrency
{
  val defaultNum: Int =
    Runtime.getRuntime.availableProcessors

  def fixedPoolWith(num: Int): IO[ExecutorService] =
    IO(Executors.newFixedThreadPool(num))

  def ec(pool: IO[ExecutorService]): Resource[IO, ExecutionContext] =
    Resource.make(pool)(es => IO(es.shutdown()))
      .map(ExecutionContext.fromExecutorService)

  def fixedPool: IO[ExecutorService] =
    fixedPoolWith(defaultNum)

  def fixedPoolEc: Resource[IO, ExecutionContext] =
    ec(fixedPool)

  def fixedPoolEcWith(num: Int): Resource[IO, ExecutionContext] =
    ec(fixedPoolWith(num))

  def fixedPoolEcStreamWith(num: Int): Stream[IO, ExecutionContext] =
    Stream.resource(fixedPoolEcWith(num))

  def fixedPoolEcStream: Stream[IO, ExecutionContext] =
    Stream.resource(fixedPoolEc)

  def cs(pool: IO[ExecutorService]): Resource[IO, ContextShift[IO]] =
    ec(pool)
      .map(IO.contextShift(_))

  def fixedPoolCsWith(num: Int): Resource[IO, ContextShift[IO]] =
    cs(fixedPoolWith(num))

  def fixedPoolCs: Resource[IO, ContextShift[IO]] =
    cs(fixedPool)

  def fixedPoolCsStreamWith(num: Int): Stream[IO, ContextShift[IO]] =
    Stream.resource(fixedPoolCsWith(num))

  def fixedPoolCsStream: Stream[IO, ContextShift[IO]] =
    Stream.resource(fixedPoolCs)

  def blocker: Resource[IO, Blocker] =
    Concurrency.fixedPoolEc.map(Blocker.liftExecutionContext)

  def blockerStream: Stream[IO, Blocker] =
    Stream.resource(blocker)
}
