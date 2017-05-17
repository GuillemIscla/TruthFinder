import Language._

trait World[T <: World[T]] {
  def findTruth(raw_text:String, oneSolution:Boolean):List[Truth[T]] = {
    val (socratesTruth, text) = parseText(raw_text)
    TruthFinder.compareTextAndTruth(socratesTruth, text, oneSolution)
  }

  def getPossibles(s:Reference[_]):List[TruthPiece[State]] = s match {
    case war:WorldAspectReference[T, WorldState[T]]@unchecked =>
      possibleWorldStates(Some(war)).map(ws => WorldAspect(war, Some(ws)))
    case name:Name =>
      races.map(r => Character(name, Some(r)))
    case _ =>
      List()
  }

  def possibleWorldStates(war:Option[WorldAspectReference[T, WorldState[T]]]):List[WorldState[T]]

  def parseText(raw_text:String):(Truth[T], List[Sentence[T]])
  protected val races:List[Race]
}
