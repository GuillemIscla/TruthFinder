package TruthEngine

import TruthEngine.Language._

trait World[W <: World[W]] {
  val worldInstance:W
  val name:String
  val description:String
  def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None):List[WorldState[W]]
  def possibleWorldAspects(ws:Option[WorldState[W]] = None):List[WorldAspectReference[W, WorldState[W]]]
  def checkWorldState(truth:Truth[W]):Boolean
  val races:List[Race]

  override def toString:String = Parser.printWorld(worldInstance)

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

  protected def findStates(truth:Truth[_], reference:Reference[State]):List[State] =
    for {
      truthPiece <- truth.truthPieces.collect{case tp if tp.reference == reference => tp}
      truthState <- truthPiece.state
    } yield truthState

  protected def findCharOfRace(truth:Truth[_], race:Race):List[Name] =
    truth.truthPieces.collect{case ch:Character if ch.state.contains(race) => ch.reference}
}
