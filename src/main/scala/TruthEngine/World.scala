package TruthEngine

import TruthEngine.Language._

trait World[W <: World[W]] {
  val worldInstance:W
  val name:String
  val description:String
  def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None):List[WorldState[W]]
  def possibleWorldAspects(ws:Option[WorldState[W]] = None):List[WorldAspectReference[W, WorldState[W]]]
  val races:List[Race]
  val truthSpeakerSentences:List[Sentence]

  override def toString:String = Parser.printWorld(worldInstance)

  def findTruth(raw_text:List[String], oneSolution:Boolean):String =
    Parser.parseText(worldInstance, raw_text).map {
      case (socratesTruth, text) =>
        Truth.compareTextAndTruth(socratesTruth, truthSpeakerSentences ++ text)
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

  //The sentences of the TruthSpeaker describe the world as they are added to every conversation and they are true
  case object TruthSpeakerRace extends Race {
    def stringRef:String = "TruthSpeaker"
    def description:String = "Always speaks the truth"
    def personality(truth: Truth[_]): Boolean => Boolean = b => b
  }
  case object TruthSpeakerRef extends Reference[Race]

  case object TruthSpeaker extends TruthPiece[Race]{
    def reference: Reference[Race] = TruthSpeakerRef
    def state: Option[Race] = Some(TruthSpeakerRace)

    def merge(other:TruthPiece[_]):TruthPiece[Race] = this
  }
  val truthSpeaker = TruthSpeaker
}
