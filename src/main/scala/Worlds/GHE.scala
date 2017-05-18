package Worlds

import TruthEngine.Language._
import TruthEngine.{Truth, World}

object GHE extends GHE

trait GHE extends World[GHE] {
  val worldInstance:GHE = GHE
  val description:String = "This world contains virtuous gods, vicious evils and voluble humans"

  def possibleWorldStates(war:Option[WorldAspectReference[GHE, WorldState[GHE]]]):List[WorldState[GHE]] =
    war match {
      case _ =>
        List(Day, Night)
    }

  def possibleWorldAspects(ws:Option[WorldState[GHE]]):List[WorldAspectReference[GHE, WorldState[GHE]]] =
    ws match {
      case _ =>
        List(DayNightReference)
    }

  val races:List[Race] = List(God, Human, Evil)


  sealed trait DayNightState extends WorldState[GHE]
  case object Day extends DayNightState{
    val stringRef:String = "Day"
    val description:String = "Days are nice, favorable for good behaviour"
  }
  case object Night extends DayNightState{
    val stringRef:String = "Night"
    val description:String = "Nights are scary, favorable for bad behaviour"
  }


  sealed trait DayNightReference extends WorldAspectReference[GHE, DayNightState]
  case object DayNightReference extends DayNightReference

  case object Human extends Race {
    val stringRef:String = "Human"
    val description:String = "Humans speak the truth during the day but they lie at night"

    def personality(truth:Truth[_]):Boolean => Boolean =
      (for {
        dayNightTruth <- truth.truthPieces.find(_.reference == DayNightReference)
        dayOrNight <- dayNightTruth.state
      } yield dayOrNight)
      .fold[Boolean => Boolean](//In case is Human it depends either on the DayNight
        _ => true //If we don't know DayNightState, whatever a Human says can be true
      )({
        case Day => //During the day Humans tell the truth
          b => b
        case Night => //During the night Humans lie
          b => !b
      })

  }

  case object Evil extends Race {
    val stringRef:String = "Evil"
    val description:String = "Evils always lie"

    def personality(truth:Truth[_]):Boolean => Boolean =
      b => !b //Evil always lie
  }

  case object God extends Race {
    val stringRef:String = "God"
    val description: String = "Gods always speak the truth"

    def personality(truth:Truth[_]):Boolean => Boolean =
      b => b //God always speak the truth
  }
}
