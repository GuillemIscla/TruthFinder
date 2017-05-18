package Worlds

import TruthEngine.Language._
import TruthEngine.{Truth, World}

object Heaven extends Heaven

trait Heaven extends World[Heaven] {
  val worldInstance:Heaven = Heaven
  val name: String = "Heaven"
  val description:String = "This world is perfect and everyone speaks the truth. There is one single God."

  def possibleWorldStates(war:Option[WorldAspectReference[Heaven, WorldState[Heaven]]]):List[WorldState[Heaven]] =
    war match {
      case _ =>
        List(Shiny)
    }

  def possibleWorldAspects(ws:Option[WorldState[Heaven]]):List[WorldAspectReference[Heaven, WorldState[Heaven]]] =
    ws match {
      case _ =>
        List(ShinyReference)
    }

  val races:List[Race] = List(God, Angel, BlessedSoul)
  val truthSpeakerSentences:List[Sentence] = List(Sentence(TruthSpeakerRef, NumberOfPeople(1, isExact = true), God, sentenceAffirmation = true, directObjectAffirmation = true))


  trait HeavenCitizen extends Race {
    def personality(truth:Truth[_]):Boolean => Boolean =
      b => b //HeavenCitizen always speak the truth
  }

  sealed trait ShinyState extends WorldState[Heaven]
  sealed trait ShinyReference extends WorldAspectReference[Heaven, ShinyState]
  case object ShinyReference extends ShinyReference
  case object Shiny extends ShinyState{
    val stringRef:String = "Shiny"
    val description:String = "Yes, shiny, shiny... It is always shiny."
  }

  case object God extends HeavenCitizen {
    val stringRef:String = "God"
    val description: String = "There is one single God and he speaks the truth."
  }

  case object Angel extends HeavenCitizen {
    val stringRef:String = "Angel"
    val description: String = "They like it here so much that they always speak the truth."
  }

  case object BlessedSoul extends HeavenCitizen {
    val stringRef:String = "BlessedSoul"
    val description: String = "They like it here so much that they always speak the truth."
  }
}
