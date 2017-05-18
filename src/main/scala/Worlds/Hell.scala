import TruthEngine.Language._
import TruthEngine.{Truth, World}

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

  val races:List[Race] = List(Lucifer, Deamon, SinnerSoul)
  val truthSpeakerSentences:List[Sentence] = List(Sentence(TruthSpeakerRef, NumberOfPeople(1, isExact = true), Lucifer, sentenceAffirmation = true, directObjectAffirmation = true))


  trait HellCitizen extends Race {
    def personality(truth:Truth[_]):Boolean => Boolean =
      b => !b //HellCitizen always lie
  }

  sealed trait GloomyState extends WorldState[Hell]
  sealed trait GloomyReference extends WorldAspectReference[Hell, GloomyState]
  case object GloomyReference extends GloomyReference
  case object Gloomy extends GloomyState{
    val stringRef:String = "Gloomy"
    val description:String = "Yes, gloomy, gloomy... It is never shiny."
  }

  case object Lucifer extends HellCitizen {
    val stringRef:String = "Lucifer"
    val description: String = "There is one single Lucifer and he always lies."
  }

  case object Deamon extends HellCitizen {
    val stringRef:String = "Deamon"
    val description: String = "They hate it here so much that they always lie."
  }

  case object SinnerSoul extends HellCitizen {
    val stringRef:String = "SinnerSoul"
    val description: String = "They hate it here so much that they always lie."
  }
}
