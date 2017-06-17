package Worlds

import TruthEngine.Language._
import TruthEngine._
import ParserHelper._
import TruthEngine.Truth.Truth

object SinnerOrSaint extends SinnerOrSaint

trait SinnerOrSaint extends World[SinnerOrSaint] {
  val worldInstance:SinnerOrSaint = SinnerOrSaint
  val name: String = "SinnerOrSaint"
  val description:String = "In this world there are always Angels or Devils (but never at the same time) and they compete for human souls."

  def possibleWorldStates(war:Option[WorldAspectReference[SinnerOrSaint, WorldState[SinnerOrSaint]]], conversation:List[Sentence] = List()):List[WorldState[SinnerOrSaint]] =
    war match {
      case _ =>
        List(AngelPresence, DaemonPresence)
    }

  def possibleWorldAspects(ws:Option[WorldState[SinnerOrSaint]], conversation:List[Sentence] = List()):List[WorldAspectReference[SinnerOrSaint, WorldState[SinnerOrSaint]]] =
    ws match {
      case _ =>
        List(SuperNaturalPresenceReference)
    }

  def checkConsistency(truth:Truth):Boolean =
    findState(truth, SuperNaturalPresenceReference) match {
      case Some(AngelPresence) =>
        findCharsOfRace(truth, Daemon).isEmpty
      case Some(DaemonPresence) =>
        findCharsOfRace(truth, Angel).isEmpty
      case _ =>
        true
    }

  def races(text:List[Sentence] = List()):List[Race] = List(Angel, Daemon, Human(Some(Saint)), Human(Some(Undecided)), Human(Some(Sinner)))

  override lazy val customParsers:List[LineParser[SinnerOrSaint]] = List(SinnerOrSaintParser)
  case object SinnerOrSaintParser extends LineParser[SinnerOrSaint] {
    val forbiddenNames: List[String] = List("feel", "feels", "like", "likes")
    val world: SinnerOrSaint = worldInstance
    val parserName: String = "SinnerOrSaintParser"

    def translate(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (It) (feels there is some) (\w+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, _, _, raw_directObject) =>
          for {
            directObject <- parseDirectObject(raw_directObject)
          } yield Sentence(Name(speaker), SuperNaturalPresenceReference, None, directObject, directObjectAffirmation = true)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseDirectObject(raw_directObject:String):Translation[String, DirectObject] = {
      raw_directObject match {
        case AngelPresence.stringRef =>
          Translated(StateDirectObject(AngelPresence))
        case DaemonPresence.stringRef =>
          Translated(StateDirectObject(DaemonPresence))
        case otherPresence =>
          TranslationError(otherPresence, s"$otherPresence is not a valid presence in the SinnerOrSaint world")
      }
    }
  }

  override lazy val customPrinters:List[TruthPiecePrinter] = List(SinnerOrSaintPrinter)
  case object SinnerOrSaintPrinter extends TruthPiecePrinter {
    def translate(raw_script_sentence: TruthPiece[State]): Translation[TruthPiece[State], String] =
      raw_script_sentence match {
        case Character(Name(charName), Some(Human(Some(Saint)))) =>
          Translated(s"$charName is Human and he is a holy Saint :D")
        case Character(Name(charName), Some(Human(Some(Undecided)))) =>
          Translated(s"$charName is Human and he is Undecided :O")
        case Character(Name(charName), Some(Human(Some(Sinner)))) =>
          Translated(s"$charName is Human and he is a shameful Sinner :'(")
        case Character(Name(charName), Some(Human(None))) =>
          Translated(s"$charName is Human but we don't know his status (¿?)")
        case WorldAspect(_, Some(presence)) =>
          Translated(s"There is some $presence")
        case _ =>
          NotTranslated(raw_script_sentence)
      }
  }

  override def customMerge(tp1:TruthPiece[State], tp2:TruthPiece[State]): Option[TruthPiece[State]] =
    (tp1, tp2) match {
      case (Character(reference1, Some(Human(Some(Saint)))), Character(reference2, Some(Human(Some(Saint))))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Human(Some(Saint)))))
      case (Character(reference1, Some(Human(Some(Undecided)))), Character(reference2, Some(Human(Some(Undecided))))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Human(Some(Undecided)))))
      case (Character(reference1, Some(Human(Some(Sinner)))), Character(reference2, Some(Human(Some(Sinner))))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Human(Some(Sinner)))))
      case (Character(reference1, Some(Human(_))), Character(reference2, Some(Human(_)))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Human(None))))
      case _ =>
        None
    }

  sealed trait SuperNaturalPresence extends WorldState[SinnerOrSaint]
  case object AngelPresence extends SuperNaturalPresence{
    val stringRef:String = "AngelPresence"
    val description:String = "When there is an angel around there are no daemons and everybody speaks the truth."
  }
  case object DaemonPresence extends SuperNaturalPresence{
    val stringRef:String = "DaemonPresence"
    val description:String = "When there is an daemon around there are no angels and lies are spoken."
  }

  sealed trait SuperNaturalPresenceReference extends WorldAspectReference[SinnerOrSaint, SuperNaturalPresence]
  case object SuperNaturalPresenceReference extends SuperNaturalPresenceReference

  sealed trait HumanStatus
  case object Saint extends HumanStatus
  case object Undecided extends HumanStatus
  case object Sinner extends HumanStatus

  case class Human(status:Option[HumanStatus]) extends Race {
    val stringRef:String = "Human"
    val description:String = "Humans can be either Saints who always tell the truth, Sinners that always lie when there is a daemon around and Undecided which only lie if they speak after a daemon"

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      (status, findState(truth, SuperNaturalPresenceReference).map(_ == AngelPresence), spokenAfterDaemon(truth, text, sentenceIndex)) match {
        case (Some(Saint), _, _) =>
          b => b
        case (_, Some(true), _) =>
          b => b
        case (Some(Sinner), Some(false), _) =>
          b => !b
        case (Some(Sinner), None, _) =>
          _ => true
        case (Some(Undecided), _, Some(true)) =>
          b => !b
        case (Some(Undecided), _, _) =>
          _ => true
        case (None, _, _) =>
          _ => true
      }

    private def spokenAfterDaemon(truth: Truth, text:List[Sentence], sentenceIndex:Int):Option[Boolean] = {
      if(sentenceIndex == 0)
        Some(false)
      else
        truth.find(tp => tp.reference == text(sentenceIndex -1).speaker).map(_.state.contains(Daemon))
    }
  }

  case object Angel extends Race {
    val stringRef:String = "Angel"
    val description: String = "They fill the environment with their presence and make everyone speak the truth."

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => b
  }

  case object Daemon extends Race {
    val stringRef:String = "Daemon"
    val description:String = "They fill the environment with their presence and force people to lie."

    def canSay(truth: Truth, text:List[Sentence], sentenceIndex:Int):Boolean => Boolean =
      b => !b
  }

}
