package Worlds

import TruthEngine.Language._
import TruthEngine.Truth.Truth
import TruthEngine._

object Poker extends Poker

trait Poker extends World[Poker] {
  val worldInstance:Poker = Poker
  val name: String = "Poker"
  val description:String = "This world shows you how it is living as a Race."

  def possibleWorldStates(war:Option[WorldAspectReference[Poker, WorldState[Poker]]]):List[WorldState[Poker]] =
    war match {
      case _ =>
        List(Diamonds, Clubs, Hearts, Spades)
    }

  def possibleWorldAspects(ws:Option[WorldState[Poker]]):List[WorldAspectReference[Poker, WorldState[Poker]]] =
    ws match {
      case _ =>
        List(SuitReference)
    }

  def checkConsistency(truth:Truth):Boolean =
    truth.count(_.state.contains(Joker)) <= 1

  val races:List[Race] = List(Ace, King, Queen, Prince, Joker) ++ (2 until 10).map(v => Number(Some(v)))

  sealed trait Suit extends WorldState[Poker] {
    val unicodeSymbol:String
  }
  case object Diamonds extends Suit{
    val stringRef:String = "Diamonds"
    val unicodeSymbol:String = "♦"
    val description:String = s"This is the unicode symbol of Diamonds: $unicodeSymbol"
  }

  case object Clubs extends Suit{
    val stringRef:String = "Clubs"
    val unicodeSymbol:String = "♣"
    val description:String = s"This is the unicode symbol of Clubs: $unicodeSymbol"
  }

  case object Hearts extends Suit{
    val stringRef:String = "Hearts"
    val unicodeSymbol:String = "♥"
    val description:String = s"This is the unicode symbol of Hearts: $unicodeSymbol"
  }

  case object Spades extends Suit{
    val stringRef:String = "Spades"
    val unicodeSymbol:String = "♠"
    val description:String = s"This is the unicode symbol of Spades: $unicodeSymbol"
  }

  sealed trait SuitReference extends WorldAspectReference[Poker, Suit]
  case object SuitReference extends SuitReference

  override lazy val  customParsers: List[LineParser[Poker]] = List(PokerParser)
  case object PokerParser extends LineParser[Poker] {
    val forbiddenNames: List[String] = List("the", "suit", "is")
    val world: Poker = worldInstance
    val parserName: String = "PokerParser"

    def translate(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (The suit) (is) (\w+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, _, _, raw_directObject) =>
          for {
            directObject <- parseDirectObject(raw_directObject)
          } yield Sentence(Name(speaker), SuitReference, directObject, directObjectAffirmation = true)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseDirectObject(raw_directObject: String): Translation[String, WorldState[Poker]] = {
      raw_directObject match {
        case Diamonds.stringRef =>
          Translated(Diamonds)
        case Spades.stringRef =>
          Translated(Spades)
        case Hearts.stringRef =>
          Translated(Hearts)
        case Clubs.stringRef =>
          Translated(Clubs)
        case otherSuit =>
          TranslationError(otherSuit, s"$otherSuit is not a valid suit in Poker")
      }
    }
  }

  override lazy val customPrinters: List[TruthPiecePrinter] = List(PokerPrinter)

  case object PokerPrinter extends TruthPiecePrinter {
    def translate(raw_script_sentence: TruthPiece[State]): Translation[TruthPiece[State], String] =
      raw_script_sentence match {
        case Character(Name(charName), Some(Number(Some(value)))) =>
          Translated(s"$charName is a Number $value")
        case Character(Name(charName), Some(Number(None))) =>
          Translated(s"$charName is a Number but we don't know its value")
        case WorldAspect(_, Some(suit:Suit)) =>
          Translated(s"The suit is ${suit.unicodeSymbol}")
        case _ =>
          NotTranslated(raw_script_sentence)
      }
  }

  override def customMerge(tp1:TruthPiece[State], tp2:TruthPiece[State]): Option[TruthPiece[State]] =
    (tp1, tp2) match {
      case (Character(reference1, Some(Number(Some(value1)))), Character(reference2, Some(Number(Some(value2))))) if reference1 == reference2 && value1 == value2=>
        Some(Character(reference1, Some(Number(Some(value1)))))
      case (Character(reference1, Some(Number(_))), Character(reference2, Some(Number(_)))) if reference1 == reference2 =>
        Some(Character(reference1, Some(Number(None))))
      case _ =>
        None
    }

  case object Ace extends Race {
    val stringRef: String = "Ace"
    val description: String = "Aces are so cool! Always speak the truth, and if others speak about him, they speak the truth... Well, with one exception."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      b => b
  }

  //subject no references al character de la frase, només és el nom. Falta el cas que Race == None
  case object King extends Race {
    val stringRef: String = "King"
    val description: String = "Along with Aces Kings are the only ones who can speak the truth about the Suit because they decide it ;) others simply don't know. Kings only speaks the truth about other Kings... And Aces of course! They lie when they speak about anyone else."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      truth.find(tp => tp.reference == text(sentenceIndex).subject).map[Boolean => Boolean]{
        case Character(_, Some(Joker) | None) =>
          _ => true
        case Character(_, Some(Ace) | Some(King)) =>
          b => b
        case _:WorldAspect[_,_] =>
          b => b
        case _ =>
          b => !b
      }.getOrElse(_ => true)
  }

  case object Queen extends Race {
    val stringRef: String = "Queen"
    val description: String = "Queens only speaks the truth about Kings and other Queens... And Aces of course! They lie when they speak about anyone else."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      truth.find(tp => tp.reference == text(sentenceIndex).subject).map[Boolean => Boolean]{
        case Character(_, Some(Joker) | None) =>
          _ => true
        case Character(_, Some(Ace) | Some(King) | Some(Queen)) =>
          b => b
        case _:WorldAspect[_,_] =>
          b => b
        case _ =>
          b => !b
      }.getOrElse(_ => true)
  }

  case object Prince extends Race {
    val stringRef: String = "Prince"
    val description: String = "Princes only speaks the truth about Kings, Queens and other Princes... And Aces of course! They lie when they speak about anyone else."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      truth.find(tp => tp.reference == text(sentenceIndex).subject).map[Boolean => Boolean]{
        case Character(_, Some(Joker) | None) =>
          _ => true
        case Character(_, Some(Ace) | Some(King) | Some(Queen) | Some(Prince)) =>
          b => b
        case _:WorldAspect[_,_] =>
          b => b
        case _ =>
          b => !b
      }.getOrElse(_ => true)
  }

  case class Number(value:Option[Int]) extends Race {
    val stringRef: String = "Number" + value.fold("")(i => s" $i")
    val description: String = "Humble numbers always speak the truth."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      truth.find(tp => tp.reference == text(sentenceIndex).subject).map[Boolean => Boolean]{
        case Character(_, Some(Joker) | None) =>
          _ => true
        case _:WorldAspect[_,_] =>
          _ => true
        case _ =>
          b => b
      }.getOrElse(_ => true)
  }

  case object Joker extends Race {
    val stringRef: String = "Joker"
    val description: String = "Joker is a total tricker! He disguises so if they speak about him, they speak the truth... is just he is cheating them! And himself is lying all the time, even about Aces or the Suit. Oh, but if talking about himself he speaks the truth. Luckily there is at most only one of them."

    def personality(truth: Truth, text: List[Sentence], sentenceIndex: Int): Boolean => Boolean =
      truth.find(tp => tp.reference == text(sentenceIndex).subject).map[Boolean => Boolean]{
        case Character(_, Some(Joker)) =>
          b => b
        case _ =>
          b => !b
      }.getOrElse(_ => true)
  }

}
