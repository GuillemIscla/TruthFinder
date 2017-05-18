package TruthEngine

import TruthEngine.Language._

trait World[W <: World[W]] {
  val worldInstance:W
  val description:String
  def findTruth(raw_text:List[String], oneSolution:Boolean):String =
    Parser.parseText(worldInstance, raw_text).map {
      case (socratesTruth, text) =>
        Truth.compareTextAndTruth(socratesTruth, text)
    } match {
      case Right(listTruth) =>
        Parser.printResult(listTruth, oneSolution)
      case Left(error) =>
        s"There has been an error in the input: $error"
    }

  def possibleTruthPieces(s:Reference[_]):List[TruthPiece[State]] = s match {
    case war:WorldAspectReference[W, WorldState[W]]@unchecked =>
      possibleWorldStates(Some(war)).map(ws => WorldAspect(war, Some(ws)))
    case name:Name =>
      races.map(r => Character(name, Some(r)))
    case _ =>
      List()
  }

  def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None):List[WorldState[W]]
  def possibleWorldAspects(ws:Option[WorldState[W]] = None):List[WorldAspectReference[W, WorldState[W]]]
  val races:List[Race]

}
