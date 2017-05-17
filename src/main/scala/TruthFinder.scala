import Language._

object TruthFinder {

  def compareTextAndTruth[W <: World[W]](truth: Truth[W], text: List[Sentence[W]], mergeSolutions:Boolean = true):List[Truth[W]]= {
    truth.nextAssumptions() match {
      case Nil => //If there are no assumptions to make we know everything
        List(truth)
      case assumptions =>
        //We don't need to iterate nodes which already are contradictory. So we filter and recursive call.
        assumptions.filter(ass => text.forall(ass.charCanSay)).flatMap(compareTextAndTruth(_, text, mergeSolutions)) match {
          case Nil => //If all the assumptions have contradiction, we return a contradiction
            Nil
          case firstAssumption::tail => //With one or several assumptions, we merge the results
            if(mergeSolutions)
              List(tail.foldLeft(firstAssumption)(Truth.merge))
            else
              firstAssumption::tail
        }
    }
  }
}
