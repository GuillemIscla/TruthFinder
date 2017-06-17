package Worlds

import TruthEngine.Language._
import TruthEngine._
import ParserHelper._
import TruthEngine.Truth.Truth

object GHE extends GHE

trait GHE extends World[GHE] {
  val worldInstance:GHE = GHE
  val name: String = "GHE"
  val description:String = "This world contains virtuous gods, vicious evils and voluble humans."

  def possibleWorldStates(war:Option[WorldAspectReference[GHE, WorldState[GHE]]], conversation:List[Sentence] = List()):List[WorldState[GHE]] =
    war match {
      case _ =>
        List(Day, Night)
    }

  def possibleWorldAspects(ws:Option[WorldState[GHE]], conversation:List[Sentence] = List()):List[WorldAspectReference[GHE, WorldState[GHE]]] =
    ws match {
      case _ =>
        List(DayNightReference)
    }

  def checkConsistency(truth:Truth):Boolean = true //No state to be checked

  def races(conversation:List[Sentence] = List()):List[Race] = List(God, Human, Evil)

  sealed trait DayNightState extends WorldState[GHE]
  case object Day extends DayNightState{
    val stringRef:String = "Day"
    val description:String = "Days are nice, favorable for good behaviours."
  }
  case object Night extends DayNightState{
    val stringRef:String = "Night"
    val description:String = "Nights are scary, favorable for bad behaviours."
  }

  sealed trait DayNightReference extends WorldAspectReference[GHE, DayNightState]
  case object DayNightReference extends DayNightReference

  case object Human extends Race {
    val stringRef:String = "Human"
    val description:String = "Humans speak the truth during the day but they lie at night."

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      findState(truth, DayNightReference) match {
        case Some(Day) => //During the day Humans tell the truth
          b => b
        case Some(Night) => //During the night Humans lie
          b => !b
        case _ =>
          _ => true //If we don't know DayNightState, whatever a Human says can be true
      }
  }

  case object Evil extends Race {
    val stringRef:String = "Evil"
    val description:String = "Evils always lie."

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => !b //Evil always lie
  }

  case object God extends Race {
    val stringRef:String = "God"
    val description: String = "Gods always speak the truth."

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => b //God always speak the truth
  }
}
