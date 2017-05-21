package TruthEngine

import TruthEngine.Language._
import TruthEngine.ParserHelper._
import TruthEngine.PrinterHelper._

trait World[W <: World[W]] {
  val worldInstance:W
  val name:String
  val description:String
  def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None):List[WorldState[W]]
  def possibleWorldAspects(ws:Option[WorldState[W]] = None):List[WorldAspectReference[W, WorldState[W]]]
  def checkWorldState(truth:Truth[W]):Boolean
  val races:List[Race]
  val customParsers:List[Parser[W]] = List()
  val customPrinters:List[Printer] = List()
  def customMerge(tp1:TruthPiece[State], tp2:TruthPiece[State]): Option[TruthPiece[State]] = None

  def printer:Printer = PrinterCollection(customPrinters ++ List(FinalPrinter))

  def findTruth(raw_text:List[String]):Translation[List[String], List[Truth[W]]] =
    ParserCollection(worldInstance, RegularWorldState(worldInstance) :: GenericParser(worldInstance) :: customParsers).translateFullScript(raw_text).map {
      case (socratesTruth, text) =>
        Truth.compareTextAndTruth(socratesTruth, text)
    }

  def possibleTruthPieces(s:Reference[_]):List[TruthPiece[State]] = s match {
    case war:WorldAspectReference[W, WorldState[W]]@unchecked =>
      possibleWorldStates(Some(war)).map(ws => WorldAspect(war, Some(ws)))
    case name:Name =>
      races.map(r => Character(name, Some(r)))
    case _ =>
      List()
  }

  protected def findState(truth:Truth[_], reference:Reference[State]):Option[State] =
    (for {
      truthPiece <- truth.truthPieces.collect{case tp if tp.reference == reference => tp}
      truthState <- truthPiece.state
    } yield truthState).headOption

  protected def findCharsOfRace(truth:Truth[_], race:Race):List[Name] =
    truth.truthPieces.collect{case ch:Character if ch.state.contains(race) => ch.reference}
}
