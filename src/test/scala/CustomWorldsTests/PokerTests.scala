package CustomWorldsTests

import TruthEngine.Language.{Character, Name, Sentence, WorldAspect}
import TruthEngine.{Translated, TranslationError}
import Worlds.Poker
import Worlds.Poker._

class PokerTests extends CustomWorldTests[Poker]{
  def world:Poker = Poker
  test("Conversations in Poker"){
    findTruthInConversation(2) should be (Translated(List(List(WorldAspect(SuitReference,Some(Diamonds)), Character(Name("A"),Some(King))))))
    findTruthInConversation(4) should be (Translated(List(List(WorldAspect(SuitReference,Some(Diamonds)), Character(Name("A"),Some(King)), Character(Name("B"),Some(Joker))))))
  }

  test("Custom parsing in Poker"){
    PokerParser.translate("A: The suit is Swords") should be (TranslationError("Swords", "Swords is not a valid suit in Poker"))
    PokerParser.translate("A: The suit is Diamonds") should be (Translated(Sentence(Name("A"),SuitReference,Diamonds, directObjectAffirmation = true)))
  }

  test("Custom print in Poker"){
    PokerPrinter.translate(WorldAspect(SuitReference, Some(Diamonds))) should be (Translated("The suit is ♦"))
    PokerPrinter.translate(WorldAspect(SuitReference, Some(Clubs))) should be (Translated("The suit is ♣"))
    PokerPrinter.translate(WorldAspect(SuitReference, Some(Hearts))) should be (Translated("The suit is ♥"))
    PokerPrinter.translate(WorldAspect(SuitReference, Some(Spades))) should be (Translated("The suit is ♠"))

    PokerPrinter.translate(Character(Name("A"), Some(Number(Some(2))))) should be (Translated("A is a Number 2"))
    PokerPrinter.translate(Character(Name("A"), Some(Number(None)))) should be (Translated("A is a Number but we don't know its value"))
  }

  test("Custom merge in Poker"){
    val num2 = Character(Name("A"), Some(Number(Some(2))))
    val num3 = Character(Name("A"), Some(Number(Some(3))))
    val undefNum = Character(Name("A"), Some(Number(None)))
    val king = Character(Name("A"), Some(King))

    Poker.customMerge(num2, num2) should be (Some(num2))
    Poker.customMerge(num2, num3) should be (Some(undefNum))
    Poker.customMerge(num2, undefNum) should be (Some(undefNum))
    Poker.customMerge(num2, king) should be (None)
  }
}
