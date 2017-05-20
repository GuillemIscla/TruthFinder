package Worlds

import TruthEngine.Language._
import TruthEngine.{Truth, World}

object MagicForest extends MagicForest

trait MagicForest extends World[MagicForest] {
  val worldInstance:MagicForest = MagicForest
  val name: String = "MagicForest"
  val description:String = "Playful creatures live in the forest and any human visitor may be tricked into its magic"

  def possibleWorldStates(war:Option[WorldAspectReference[MagicForest, WorldState[MagicForest]]]):List[WorldState[MagicForest]] =
    war match {
      case Some(_:DayNightReference) =>
        List(Day, Night)
      case Some(_:MagicReference) =>
        List(WhiteMagic, BlackMagic)
      case _ =>
        List(Day, Night, WhiteMagic, BlackMagic)
    }

  def possibleWorldAspects(ws:Option[WorldState[MagicForest]]):List[WorldAspectReference[MagicForest, WorldState[MagicForest]]] =
    ws match {
      case Some(Day | Night) =>
        List(DayNightReference)
      case Some(WhiteMagic | BlackMagic) =>
        List(MagicReference)
      case _ =>
        List(DayNightReference, MagicReference)
    }

  def checkWorldState(truth: Truth[MagicForest]):Boolean =
    (findStates(truth, MagicReference).headOption match {
      case Some(WhiteMagic) =>
        findCharOfRace(truth, Goblin).isEmpty//No Goblins with WhiteMagic
      case Some(BlackMagic) =>
        findCharOfRace(truth, Fairy).isEmpty//No Fairies with BlackMagic
      case _ =>
        true
    }) &&
      (findCharOfRace(truth, HumanVisitor).length <  //There are more characters than just HumanVisitors
        truth.truthPieces.collect{case ch:Character => ch.reference}.length)


  val races:List[Race] = List(HumanVisitor, Wizard, Fairy, Goblin)

  sealed trait MagicState extends WorldState[MagicForest]
  case object WhiteMagic extends MagicState{
    val stringRef:String = "WhiteMagic"
    val description:String = "WhiteMagic can be used by Wizards to promote good behaviours. Goblins hate WhiteMagic."
  }
  case object BlackMagic extends MagicState{
    val stringRef:String = "BlackMagic"
    val description:String = "BlackMagic can be used by Wizards to promote bad behaviours. Fairies hate BlackMagic"
  }


  sealed trait MagicReference extends WorldAspectReference[MagicForest, MagicState]
  case object MagicReference extends MagicReference

  sealed trait DayNightState extends WorldState[MagicForest]
  case object Day extends DayNightState{
    val stringRef:String = "Day"
    val description:String = "Days are nice, favorable for good behaviours."
  }
  case object Night extends DayNightState{
    val stringRef:String = "Night"
    val description:String = "Nights are scary, favorable for bad behaviours."
  }

  sealed trait DayNightReference extends WorldAspectReference[MagicForest, DayNightState]
  case object DayNightReference extends DayNightReference

  trait Human extends Race {
    def humanPersonality(dayOrNight:Option[State]):Boolean => Boolean =
      dayOrNight match {
        case Some(Day) => //During the day Humans tell the truth
          b => b
        case Some(Night) => //During the night Humans lie
          b => !b
        case _ =>
          _ => true //If we don't know DayNightState, whatever a Human says can be true
      }
  }

  case object HumanVisitor extends Human {
    val stringRef:String = "HumanVisitor"
    val description:String = "They speak the truth during the day but they lie at night... unless there is a Wizard! Then their personality changes according to the magic used. The forest can accept some of them but at least there is someone who is not HumanVisitor."
    def personality(truth: Truth[_], text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
        if(findCharOfRace(truth, Wizard).nonEmpty) //If there is a Wizard we need to check if he is using WhiteMagic or BlackMagic
          findStates(truth, MagicReference).headOption match {
            case Some(WhiteMagic) => //If a magician uses white magic HumanVisitors speak the truth
              b => b
            case Some(BlackMagic) => //If a magician uses black magic HumanVisitors lie
              b => !b
            case _ =>
              _ => true //If we don't know MagicState, whatever a HumanVisitor says can be true
          }
        else
          humanPersonality(findStates(truth, DayNightReference).headOption)

  }

  case object Wizard extends Human {
    val stringRef:String = "Wizard"
    val description:String = "They are Humans after all, so speak the truth during the day but they lie at night. When they are around they use the Magic available to change HumanVisitors personality. They are already magic so magic environment does not affect them."
    def personality(truth: Truth[_], text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      humanPersonality(findStates(truth, DayNightReference).headOption)
  }

  case object Fairy extends Race {
    val stringRef:String = "Fairy"
    val description:String = "They grant you the gift of the truth the first time they speak. After that they are playful and they might speak the truth or not. They only show up when there is WhiteMagic."
    def personality(truth: Truth[_], text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      if(text(sentenceIndex) == text.filter(_.speaker == text(sentenceIndex).speaker).head) //If this sentence is the first time they speak
        b => b
      else
        _ => true
  }

  case object Goblin extends Race {
    val stringRef:String = "Goblin"
    val description:String = "Last time they speak, they say a funny truth before vanishing. Any other time they are playful and they might speak the truth or not. They only show up when there is BlackMagic."
    def personality(truth: Truth[_], text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      if(text(sentenceIndex) == text.filter(_.speaker == text(sentenceIndex).speaker).last) //If this sentence is the last time they speak
        b => b
      else
        _ => true
  }
}
