package CustomWorldsTests

import TruthEngine.Language.{Character, Name, WorldAspect}
import TruthEngine.Translated
import Worlds.GHE
import Worlds.GHE._

class GHETests extends CustomWorldTests[GHE]{
  def world:GHE = GHE

  test("Conversations in GHE"){
    findTruthInConversation(1) should be (Translated(List(List(WorldAspect(DayNightReference,Some(Night)), Character(Name("A"),Some(Human))))))
    findTruthInConversation(2) should be (Translated(List(List(WorldAspect(DayNightReference,Some(Night)), Character(Name("A"),Some(God)), Character(Name("B"),Some(Human)), Character(Name("C"),Some(Human))), List(WorldAspect(DayNightReference,Some(Night)), Character(Name("A"),Some(God)), Character(Name("B"),Some(Human)), Character(Name("C"),Some(Evil))))))
    findTruthInConversation(3) should be (Translated(List(List(WorldAspect(DayNightReference,Some(Night)), Character(Name("A"),Some(God)), Character(Name("B"),Some(Human))), List(WorldAspect(DayNightReference,Some(Night)), Character(Name("A"),Some(Human)), Character(Name("B"),Some(Human))))))
  }
}
