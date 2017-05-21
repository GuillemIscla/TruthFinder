package Worlds

import TruthEngine.Language._
import TruthEngine._
import ParserHelper._

object Hell extends Hell

trait Hell extends World[Hell] {
  val worldInstance:Hell = Hell
  val name: String = "Hell"
  val description:String = "This world is horrible and everyone lies. There is one single Lucifer."

  def possibleWorldStates(war:Option[WorldAspectReference[Hell, WorldState[Hell]]]):List[WorldState[Hell]] =
    war match {
      case _ =>
        List(Gloomy)
    }

  def possibleWorldAspects(ws:Option[WorldState[Hell]]):List[WorldAspectReference[Hell, WorldState[Hell]]] =
    ws match {
      case _ =>
        List(GloomyReference)
    }

  def checkWorldState(truth: Truth[Hell]):Boolean = { //There is exactly 1 Lucifer
    val charRaces = truth.truthPieces.collect {
      case ch: Character =>
        ch.state
    }
    charRaces.exists(_.isEmpty) || charRaces.count(_.contains(Lucifer)) == 1
  }

  val races:List[Race] = List(Lucifer, Daemon, SinnerSoul)

  sealed trait GloomyState extends WorldState[Hell]
  sealed trait GloomyReference extends WorldAspectReference[Hell, GloomyState]
  case object GloomyReference extends GloomyReference
  case object Gloomy extends GloomyState{
    val stringRef:String = "Gloomy"
    val description:String = "Yes, gloomy, gloomy... It is never shiny!"
  }

  trait HellCitizen extends Race {
    def personality(truth: Truth[_], text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => !b //HellCitizen always lies
  }

  case object Lucifer extends HellCitizen {
    val stringRef:String = "Lucifer"
    val description: String = "There is one single Lucifer and he always lies."
  }

  case object Daemon extends HellCitizen {
    val stringRef:String = "Daemon"
    val description: String = "They hate it here so much that they always lie."
  }

  case object SinnerSoul extends HellCitizen {
    val stringRef:String = "SinnerSoul"
    val description: String = "They hate it here so much that they always lie."
  }
}
