import scala.collection.immutable.::

/**
  * Created by Iscle on 12/01/2017.
  */
object TruthFinder {
  def main(args: Array[String]): Unit = {
    println(findTruth("", oneSolution = false))
  }

  sealed trait Reference
  case object DayNightRef extends Reference
  case class Name(charName: String) extends Reference

  sealed trait State

  sealed trait DayNightState extends State
  case object Day extends DayNightState
  case object Night extends DayNightState

  trait Race extends State {
    def personality(truth:Truth):Boolean => Boolean //Sometimes personality might depend on states of the truth
  }

  case object Human extends Race {
    def personality(truth:Truth):Boolean => Boolean =
      truth.dayNight.state.fold[Boolean => Boolean](//In case is Human it depends either on the DayNight
        _ => true //If we don't know DayNightState, whatever a Human says can be true
      )({
        case Day => //During the day Human tells the truth
          b => b
        case Night => //During the night Human lies
          b => !b
      })

  }

  case object Evil extends Race {
    def personality(truth:Truth):Boolean => Boolean =
      b => !b //Evil always lie
  }

  case object God extends Race {
    def personality(truth:Truth):Boolean => Boolean =
      b => b //God always speaks the truth
  }

  trait TruthPiece {
    def reference: Reference
    def state: Option[State]
  }

  case class Character(charName: String, race: Option[Race]) extends TruthPiece {
    def reference: Name = Name(charName)
    def state: Option[Race] = race
  }

  case class DayNight(isDayNight: Option[DayNightState]) extends TruthPiece {
    def reference: Reference = DayNightRef
    def state: Option[DayNightState] = isDayNight
  }

  case class Sentence(subject: Reference, directObject: State, affirmative:Boolean = true) extends TruthPiece {
    def reference: Reference = subject
    def state: Some[State] = Some(directObject)
    def compareWithTruthPiece(tp:TruthPiece): Option[Boolean] =
      tp.reference match {
        case tpRef if tpRef == subject => //In case we got the sentence topic, we compare with DO
          tp.state.map(compareStateAndDO)
        case _ =>   //In case we got a TruthPiece the sentence doesn't talk about, we return None
          None
      }
    private def compareStateAndDO(s:State): Boolean =
      if(affirmative) s == directObject
      else s != directObject
  }

  object Truth {
    def merge(a:Truth, b:Truth):Truth = {//Merge two truths that preserves the minimum set of certanty
      def mergeStates[S <: State](states:(Option[S], Option[S])):Option[S] =
        states match {
          case (Some(s1), Some(s2)) if s1 == s2 =>
            Some(s1)
          case _ =>
            None
        }
      Truth(
        DayNight(mergeStates(a.dayNight.state, b.dayNight.state)),
        a.characters.zip(b.characters).map(c1c2 =>
          Character(c1c2._1.charName, mergeStates(c1c2._1.race, c1c2._2.race)) )
      )
    }
  }

  case class Truth(dayNight: DayNight, characters: List[Character]) {
    def charCanSay(characterRef: Name, sentence: Sentence): Boolean = {
      val personality: Boolean => Boolean = //Function that the given character is going to use to talk sentences to us
        characters.find(_.reference == characterRef)
          .fold[Boolean => Boolean](_ => false)(//In case there is no such character this can't happen
          _.state match {
            case Some(race) => //We retrieve teh personality of the caracter
              race.personality(this)
            case None => //If we don't know who speak a sentence can always be true
              _ => true
          }
        )

      val isSentenceTrue:Option[Boolean] = //We check if the sentence is true (we get None if we can't)
        sentence.reference match {
          case DayNightRef =>
            sentence.compareWithTruthPiece(dayNight)
          case characterRef: Name =>
            characters.find(_.reference == characterRef).flatMap(sentence.compareWithTruthPiece)
        }
      //If we don't know the boolean value of the sentence, whatever the character says offers no contradiction
      //If we know if the boolean value of the sentence, we check with the personality function if the character offer a contradiction
      isSentenceTrue.fold(true)(personality)
    }

    def nextAssumptions: List[Truth] = {
      if (dayNight.state.isDefined) {
        characters.find(_.state.isEmpty) match {
          case Some(char) =>
            val index = characters.indexOf(char)
            List(
              Truth(dayNight, characters.patch(index, Seq(Character(char.charName, Some(Human))), 1)),
              Truth(dayNight, characters.patch(index, Seq(Character(char.charName, Some(Evil))), 1)),
              Truth(dayNight, characters.patch(index, Seq(Character(char.charName, Some(God))), 1))
            )
          case None =>
            List.empty
        }
      }
      else
        List(Truth(DayNight(Some(Day)), characters), Truth(DayNight(Some(Night)), characters))
    }
  }

  case class Line(speaker: String, sentence: Sentence)

  case class Text(lines: List[Line]) {
    def check(truth: Truth): Boolean =
      lines.forall(line => truth.charCanSay(Name(line.speaker), line.sentence))
  }

  def compareTextAndTruth(truth: Truth, text: Text, mergeSolutions:Boolean = true): List[Truth] = {
    truth.nextAssumptions match {
      case Nil => //If there are no assumptions to make we know everything
        List(truth)
      case assumptions =>
        assumptions.filter(text.check).flatMap(compareTextAndTruth(_, text, mergeSolutions)) match { //We don't need to go down on a nodes which already are contradictory. So we filter and recursive call.
          case Nil => //If all the assumptions have contradiction, we return a contradiction
            Nil
          case firstAssumption::tail => //With one or several assumptions, we merge the results
            if(mergeSolutions)
              List(tail.foldLeft(firstAssumption)(Truth.merge))
            else
              firstAssumption::tail
        }
    }
  }

  //The set of things Socrates claimed to know is our basis truth
  def socratesTruth(charNames: List[String]): Truth =
    Truth(DayNight(None), charNames.map(Character(_, None)))

  def parseText(rawText: String): (List[String], Text) =
//    (List("A"), Text(List(Line("A", Sentence(Name("A"), Evil)))))
//    (List("A"),
//      Text(List(
//        Line("A", Sentence(Name("A"), Human, affirmative = false)),
//        Line("A", Sentence(DayNightRef, Night))
//      )))
//    (List("A"),Text(List(Line("A", Sentence(Name("A"), God, affirmative = false)))))
//    (List("A"),
//      Text(List(
//        Line("A", Sentence(Name("A"), Evil, affirmative = false)),
//        Line("A", Sentence(Name("A"), Human, affirmative = false)),
//        Line("A", Sentence(DayNightRef, Night))
//      )))
    (List("A"), Text(List(Line("A", Sentence(Name("A"), Human, affirmative = false)))) )
//    (List("A", "B", "C"),
//      Text(List(
//        Line("A", Sentence(Name("A"), God)),
//        Line("B", Sentence(Name("B"), God)),
//        Line("C", Sentence(Name("C"), God)),
//        Line("A", Sentence(Name("B"), Human)),
//        Line("A", Sentence(Name("C"), Evil)),
//        Line("B", Sentence(Name("A"), Human)),
//        Line("B", Sentence(Name("C"), Human)),
//        Line("C", Sentence(Name("A"), Human)),
//        Line("C", Sentence(Name("B"), God))
//      )))

//    (List("A", "B", "C"),
//      Text(List(
//        Line("A", Sentence(Name("A"), God)),
//        Line("A", Sentence(Name("B"), Evil)),
//        Line("B", Sentence(Name("B"), God)),
//        Line("B", Sentence(Name("C"), Evil)),
//        Line("C", Sentence(Name("C"), Evil)),
//        Line("C", Sentence(Name("B"), Human))
//      )))

  def findTruth(rawText: String, oneSolution:Boolean): List[Truth] = {
    val (charNames, text) = parseText(rawText)
    compareTextAndTruth(socratesTruth(charNames), text, oneSolution)
  }
}