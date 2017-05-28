package CustomWorldsTests

import TruthEngine.Language.{Character, Name, WorldAspect}
import TruthEngine.Translated
import Worlds.Hell
import Worlds.Hell._

class HellTests extends CustomWorldTests[Hell]{
  def world:Hell = Hell

  test("Conversations in Hell"){
    findTruthInConversation(1) should be (Translated(List(List(WorldAspect(GloomyReference,Some(Gloomy)), Character(Name("A"),Some(Lucifer))))))
  }
}
