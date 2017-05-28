package CustomWorldsTests

import TruthEngine.Language.{Character, Name, Sentence, WorldAspect}
import TruthEngine.{Translated, TranslationError}
import Worlds.MagicForest
import Worlds.MagicForest._

class MagicForestTests extends CustomWorldTests[MagicForest]{
  def world:MagicForest = MagicForest

  test("Conversations in MagicForest"){
    findTruthInConversation(1) should be (Translated(List(List(WorldAspect(DayNightReference,Some(Day)), WorldAspect(MagicReference,Some(BlackMagic)), Character(Name("A"),Some(Goblin))), List(WorldAspect(DayNightReference,Some(Night)), WorldAspect(MagicReference,Some(BlackMagic)), Character(Name("A"),Some(Goblin))))))
    findTruthInConversation(2) should be (Translated(List(List(WorldAspect(DayNightReference,Some(Day)), WorldAspect(MagicReference,Some(WhiteMagic)), Character(Name("A"),Some(Fairy))), List(WorldAspect(DayNightReference,Some(Night)), WorldAspect(MagicReference,Some(WhiteMagic)), Character(Name("A"),Some(Fairy))))))
  }

  test("Custom parsing in MagicForest"){
    MagicForestParser.translate("A: There is BadMood around") should be (TranslationError("BadMood", "BadMood is not a valid environment in the MagicForest"))
    MagicForestParser.translate("A: There is WhiteMagic around") should be (Translated(Sentence(Name("A"),MagicReference,WhiteMagic, directObjectAffirmation = true)))
  }

  test("Custom print in MagicForest"){
    MagicForestPrinter.translate(WorldAspect(MagicReference, Some(WhiteMagic))) should be (Translated("There is WhiteMagic around"))
  }

  test("Extra deductions in MagicForest"){
    findTruthInConversation(3).map(MagicForest.extraDeductions) should be (Translated(List("A is not a Human being")))
    findTruthInConversation(4).map(MagicForest.extraDeductions) should be (Translated(List("A is a Human being")))
  }
}
