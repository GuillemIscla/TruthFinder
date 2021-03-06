package TruthEngine

import TruthEngine.Truth.Truth

object Language {
  trait State {
    def stringRef:String
    def description:String
    def compare(other:State):Boolean = stringRef == other.stringRef
  }
  trait WorldState[+W] extends State
  trait Race extends State {
    def canSay(truthPieces: List[TruthPiece[State]], text:List[Sentence], sentenceIndex:Int): Boolean => Boolean //Sometimes personality might depend on states of the truth
  }

  trait Reference[+S <: State]
  case class Name(stringRef: String) extends Reference[Race]
  trait WorldAspectReference[W <: World[W], +WS <: WorldState[W]] extends Reference[WS]
  trait CollectiveReference extends Reference[Race]
  trait PeopleCounter
  case object Exactly extends PeopleCounter
  case object MoreOrEqual extends PeopleCounter
  case object LessOrEqual extends PeopleCounter
  case object Everyone extends CollectiveReference
  case class NumberOfPeople(number:Int, isExact:PeopleCounter) extends CollectiveReference

  sealed trait DirectObject
  case class StateDirectObject(content:State) extends DirectObject
  case class ReferenceDirectObject(content: Reference[State]) extends DirectObject
  trait NonCopulativeVerb {
    def truthPieceTrue(sentence:Sentence, truth:Truth, truthPieceIndex:Int):Option[Boolean]
  }


  trait TruthPiece[+S <: State] {
    def reference: Reference[S]
    def state: Option[S]

    def merge(other:TruthPiece[_]):TruthPiece[S]
  }
  case class Character(reference: Name, state: Option[Race]) extends TruthPiece[Race] {
    def merge(other:TruthPiece[_]):TruthPiece[Race] =
      Character(reference,
        (state, other.state) match {
          case (Some(r1), Some(r2)) if r1 == r2 =>
            state
          case _ =>
            None
        })
  }

  case class WorldAspect[W <:World[W], WS <: WorldState[W]](reference: WorldAspectReference[W, WS], state: Option[WS]) extends TruthPiece[WS]{
    def merge(other:TruthPiece[_]):TruthPiece[WS] =
      WorldAspect(reference,
        (state, other.state) match {
          case (Some(r1), Some(r2)) if r1 == r2 =>
            state
          case _ =>
            None
        })
  }

  case class Sentence(speaker:Reference[Race], subject:Reference[State], maybeNonCopulativeVerb:Option[NonCopulativeVerb], directObject:DirectObject, directObjectAffirmation:Boolean) {
    def compareWithTruth(truth: Truth): Option[Boolean] =
      subject match {
        case Everyone =>
          val everyone = truth.collect({case ch:Character  => ch})
          everyone.find(tp => !compareStateAndDirectObject(truth, truth.indexOf(tp)).getOrElse(true)) match {//looking for contradictions
            case Some(_) =>
              Some(false)//case where we could find a contradiction
            case None => //looking for places where we are not sure
              everyone.find(_.state.isEmpty) match {
                case Some(_) =>
                  None // case where we have not contradiction but we could find some place where we are not sure
                case None =>
                  Some(true) //case where we are sure that the sentence is true
              }
          }

        case NumberOfPeople(number, peopleCounter) =>
          val everyone = truth.collect({case ch:Character => ch})
          val matches = everyone.count(tp => compareStateAndDirectObject(truth, truth.indexOf(tp)).getOrElse(false))
          val unknowns = everyone.count(_.state.isEmpty)
          peopleCounter match {
            case Exactly if matches == number && unknowns == 0 =>
              Some(true) //seeking for exact number of people and we are sure that is the case
            case Exactly if matches <= number && unknowns >= number - matches=>
              None //seeking for exact number of people and we are not sure if that is the case
            case MoreOrEqual if matches >= number =>
              Some(true) //seeking for exact number of people or more and we are sure that is the case
            case MoreOrEqual if matches + unknowns >= number =>
              None //seeking for exact number of people or more and we are not sure if that is the case
            case LessOrEqual if matches + unknowns <= number =>
              Some(true) //seeking for exact number of people or less and we are sure that is the case
            case LessOrEqual if matches <= number =>
              None //seeking for exact number of people or less and we are not sure if that is the case
            case _ =>
              Some(false) //otherwise is a contradiction
          }

        case _ =>
          truth.find(_.reference == subject).flatMap(tp => compareWithTruthPiece(truth, truth.indexOf(tp)))
      }

    private def compareWithTruthPiece(truth:Truth, truthPieceIndex:Int):Option[Boolean] =
      truth(truthPieceIndex).reference match {
        case tpRef if tpRef == subject => //In case we got the sentence topic, we compare with DO
          compareStateAndDirectObject(truth, truthPieceIndex)
        case _ => //In case we got a TruthPiece the sentence doesn't talk about, we return None
          None
      }

    private def compareStateAndDirectObject(truth:Truth, truthPieceIndex:Int): Option[Boolean] =
      maybeNonCopulativeVerb match {
        case Some(nonCopulativeVerb) =>
          nonCopulativeVerb.truthPieceTrue(this, truth, truthPieceIndex)
        case None =>
          directObject match {
            case StateDirectObject(sdo) =>
              if (directObjectAffirmation) truth(truthPieceIndex).state.map(_.compare(sdo))
              else truth(truthPieceIndex).state.map(!_.compare(sdo))
            case ReferenceDirectObject(_) =>
              Some(false)
          }
      }

  }

}