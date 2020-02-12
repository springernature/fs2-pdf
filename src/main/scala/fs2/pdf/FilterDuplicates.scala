package fs2
package pdf

import cats.effect.IO
import cats.implicits._
import fs2.{Pipe, Stream}

object FilterDuplicates
{
  case class State(nums: Set[Long], duplicates: Set[Long])
  {
    def push(num: Long): (Boolean, State) = {
      val dupe = nums.contains(num)
      (dupe, if (dupe) copy(duplicates = duplicates + num) else copy(nums = nums + num))
    }
  }

  def check(state: State)(num: Long, parsed: Parsed): Pull[IO, Parsed, State] = {
    val (dupe, newState) = state.push(num)
    Pull.output1(parsed)
      .unlessA(dupe)
      .as(newState)
  }

  def filter(state: State): Parsed => Pull[IO, Parsed, State] = {
    case parsed @ Parsed.IndirectObj(Obj(Obj.Index(num, _), _), _, _) =>
      check(state)(num, parsed)
    case parsed @ Parsed.StreamObject(Obj(Obj.Index(num, _), _)) =>
      check(state)(num, parsed)
    case parsed @ Parsed.Unparsable(Obj.Index(num, _), _) =>
      check(state)(num, parsed)
    case parsed =>
      Pull.output1(parsed).as(state)
  }

  // TODO metric when duplicates are found
  def pullFilter(log: Log)(in: Stream[IO, Parsed]): Pull[IO, Parsed, Unit] =
    StreamUtil.pullState(filter)(in)(State(Set.empty, Set.empty))
      .flatMap {
        case State(_, dupes) =>
          Pull.eval(log.debug("duplicate objects in pdf").whenA(dupes.nonEmpty))
      }

  def pipe(log: Log): Pipe[IO, Parsed, Parsed] =
    pullFilter(log)(_).stream
}
