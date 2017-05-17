import Language._

object GHE extends GHE
trait GHE extends World[GHE] {

  def possibleWorldStates(war:Option[WorldAspectReference[GHE, WorldState[GHE]]]):List[WorldState[GHE]] =
    war match {
      case _ =>
        List(Day, Night)
    }

  val races:List[Race] = List(God, Human, Evil)


  sealed trait DayNightState extends WorldState[GHE]
  case object Day extends DayNightState
  case object Night extends DayNightState


  sealed trait DayNightReference extends WorldAspectReference[GHE, DayNightState]
  case object DayNightReference extends DayNightReference

  def parseText(raw_text:String):(Truth[GHE], List[Sentence[GHE]]) = (Truth(this, List(WorldAspect(DayNightReference, None), Character(Name("A"), None))), List(Sentence(Name("A"), Name("A"), Evil)))

  case object Human extends Race {
    def personality(truth:Truth[_]):Boolean => Boolean =
      (for {
        dayNightTruth <- truth.truthPieces.find(_.reference == DayNightReference)
        dayOrNight <- dayNightTruth.state
      } yield dayOrNight)
      .fold[Boolean => Boolean](//In case is Human it depends either on the DayNight
        _ => true //If we don't know DayNightState, whatever a Human says can be true
      )({
        case Day => //During the day Human tells the truth
          b => b
        case Night => //During the night Human lies
          b => !b
      })

  }

  case object Evil extends Race {
    def personality(truth:Truth[_]):Boolean => Boolean =
      b => !b //Evil always lie
  }

  case object God extends Race {
    def personality(truth:Truth[_]):Boolean => Boolean =
      b => b //God always speaks the truth
  }
}
