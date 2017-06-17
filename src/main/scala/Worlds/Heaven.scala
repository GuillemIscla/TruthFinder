package Worlds

import TruthEngine.Language._
import TruthEngine._
import ParserHelper._
import TruthEngine.Truth.Truth

object Heaven extends Heaven

trait Heaven extends World[Heaven] {
  val worldInstance:Heaven = Heaven
  val name: String = "Heaven"
  val description:String = "This world is perfect and everyone speaks the truth. There is one single God."

  def possibleWorldStates(war:Option[WorldAspectReference[Heaven, WorldState[Heaven]]], conversation:List[Sentence] = List()):List[WorldState[Heaven]] =
    war match {
      case _ =>
        List(Shiny)
    }

  def possibleWorldAspects(ws:Option[WorldState[Heaven]], conversation:List[Sentence] = List()):List[WorldAspectReference[Heaven, WorldState[Heaven]]] =
    ws match {
      case _ =>
        List(ShinyReference)
    }

  def races(conversation:List[Sentence] = List()):List[Race] = List(God, Angel, BlessedSoul)

  def checkConsistency(truth: Truth):Boolean = {
    val charRaces = truth.collect {
      case ch: Character =>
        ch.state
    }
    charRaces.exists(_.isEmpty) || charRaces.count(_.contains(God)) == 1
  }

  sealed trait ShinyState extends WorldState[Heaven]
  sealed trait ShinyReference extends WorldAspectReference[Heaven, ShinyState]
  case object ShinyReference extends ShinyReference
  case object Shiny extends ShinyState{
    val stringRef:String = "Shiny"
    val description:String = "Yes, shiny, shiny... It is never gloomy!"
  }

  trait HeavenCitizen extends Race {
    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => b //HeavenCitizen always speaks the truth
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
