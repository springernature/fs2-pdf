package fs2
package pdf

import scodec.Attempt

object AnalyzeObject
{
  def metadataObj: Obj => Analyzed = {
    case Obj(index, Prim.tpe("Page", data)) =>
      Analyzed.Page(Page(index, data))
    case obj @ Obj.tpe("Pages", data) if data.data.contains("Parent") =>
      Analyzed.Keep(obj, None)
    case Obj(index, Prim.tpe("Pages", data)) =>
      Analyzed.PageDir(PageDir(index, data))
    case Obj(index, Prim.fontResources(data, nums)) =>
      Analyzed.FontResources(FontResource(index, data), nums)
    case Obj(_, Prim.linearization()) =>
      Analyzed.Linearized
    case Obj(index, data @ Prim.Array(_)) =>
      Analyzed.IndirectArray(IndirectArray(index, data))
    case obj =>
      Analyzed.Keep(obj, None)
  }

  def parseRewritableObj(obj: Obj, stream: Option[Parsed.Stream]): Attempt[Analyzed] =
    stream.fold(Attempt.successful(metadataObj(obj)))(AnalyzeStream(_)(obj))

  def collectObjStats: Obj => Attempt[Option[Analyzed]] = {
    case Obj(_, Prim.tpe("Pages", data)) =>
      Attempt.successful(
        Prim.Dict.path("Kids")(data) { case Prim.Array(kids) => kids.collect { case Prim.Ref(n, _) => n } }
          .toOption
          .map(Analyzed.PageNumbers(_))
        )
    case Obj(_, Prim.fonts(nums)) =>
      Attempt.successful(Some(Analyzed.Fonts(nums)))
    case _ =>
      Attempt.successful(None)
  }

  def apply(obj: Obj, stream: Option[Parsed.Stream]): Attempt[List[Analyzed]] =
    for {
      rw <- parseRewritableObj(obj, stream)
      stats <- stream.fold(collectObjStats(obj))(_ => Attempt.successful(None))
    } yield rw :: stats.toList
}
