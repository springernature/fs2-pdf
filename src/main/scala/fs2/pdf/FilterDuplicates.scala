package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}

/**
  * Some broken authoring tools may leave invalid objects with duplicate numbers in the document that cause the PDF to
  * be unreadable after processing.
  * This heuristic discards later objects if their numbers have been observed before.
  * Since incremental updates produce false positives for this algorithm, because they allow overwriting previous
  * objects, the check is deactivated once an xref is encountered, since this indicates an incremental update.
  */
object FilterDuplicates
{
  private[this]
  case class State(nums: Set[Long], duplicates: Set[Long], update: Boolean)
  {
    def push(num: Long): (Boolean, State) = {
      val dupe = !update && nums.contains(num)
      (dupe, if (dupe) copy(duplicates = duplicates + num) else copy(nums = nums + num))
    }
  }

  private[this]
  def check(state: State)(num: Long, topLevel: TopLevel): Pull[IO, TopLevel, State] = {
    val (dupe, newState) = state.push(num)
    Pull.output1(topLevel)
      .unlessA(dupe)
      .as(newState)
  }

  private[this]
  def filter(state: State): TopLevel => Pull[IO, TopLevel, State] = {
    case topLevel @ TopLevel.IndirectObj(IndirectObj(Obj(Obj.Index(num, _), _), _)) =>
      check(state)(num, topLevel)
    case topLevel @ TopLevel.Xref(_) =>
      Pull.output1(topLevel).as(state.copy(update = true))
    case topLevel @ TopLevel.StartXref(_) =>
      Pull.output1(topLevel).as(state.copy(update = true))
    case topLevel =>
      Pull.output1(topLevel).as(state)
  }

  private[this]
  def pullFilter(log: Log)(in: Stream[IO, TopLevel]): Pull[IO, TopLevel, Unit] =
    StreamUtil.pullState(filter)(in)(State(Set.empty, Set.empty, false))
      .flatMap {
        case State(_, dupes, _) =>
          Pull.eval(log.debug("duplicate objects in pdf").whenA(dupes.nonEmpty))
      }

  /**
    * @param log a logger
    * @return a [[Pipe]] that suppresses output of objects if their numbers match previous objects
    */
  def pipe(log: Log): Pipe[IO, TopLevel, TopLevel] =
    pullFilter(log)(_).stream
}
