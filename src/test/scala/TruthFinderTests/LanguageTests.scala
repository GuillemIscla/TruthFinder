package TruthFinderTests

import TruthEngine.Language._
import TruthEngine.Truth.Truth
import TruthFinderTests.TestWorld._
import org.scalatest.{FunSuite, Matchers}

trait LanguageTests extends FunSuite with Matchers

class CharacterTests extends LanguageTests {

  test("Characters with same state defined state should merge into character with same parameters"){
    val char1 = Character(Name("char"), Some(TestRace("Race1")))
    val char2 = Character(Name("char"), Some(TestRace("Race1")))

    char1.merge(char2) should be (char1)
  }

  test("Characters with different state defined state should merge into character with undefined state"){
    val char1 = Character(Name("char"), Some(TestRace("Race1")))
    val char2 = Character(Name("char"), Some(TestRace("Race2")))

    char1.merge(char2) should be (Character(Name("char"), None))
  }

  test("Characters with undefined state (one or several) should merge into character with undefined state"){
    val char1 = Character(Name("char"), Some(TestRace("Race1")))
    val char2 = Character(Name("char"), None)

    char1.merge(char2) should be (Character(Name("char"), None))
    char2.merge(char1) should be (Character(Name("char"), None))
    char2.merge(char2) should be (Character(Name("char"), None))
  }

}

class WorldStateTests extends LanguageTests {

  test("WorldStates with same state defined state should merge into world state with same parameters"){
    val was1 = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1")))
    val was2 = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1")))

    was1.merge(was2) should be (was1)
  }

  test("WorldStates with different state defined state should merge into world state with undefined state"){
    val was1 = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1")))
    val was2 = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState2")))

    was1.merge(was2) should be (WorldAspect(TestWorldStateRef("worldStateRef1"), None))
  }

  test("WorldStates with same undefined state (one or several) should merge into world state with undefined state"){
    val was1 = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1")))
    val was2 = WorldAspect(TestWorldStateRef("worldStateRef1"), None)

    was1.merge(was2) should be (WorldAspect(TestWorldStateRef("worldStateRef1"), None))
    was2.merge(was1) should be (WorldAspect(TestWorldStateRef("worldStateRef1"), None))
    was2.merge(was2) should be (WorldAspect(TestWorldStateRef("worldStateRef1"), None))
  }
}

class SentenceTests extends LanguageTests {

  test("Positive Everyone sentence compared with Truths where defined characters match, no undefined") {
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive Everyone sentence compared with Truths where defined characters match, some undefined") {
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Everyone sentence compared with Truths where defined characters some match some not, no undefined") {
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Positive Everyone sentence compared with Truths where defined characters some match some not, some undefined") {
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Positive Everyone sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Everyone sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Positive Everyone sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Everyone sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative Everyone sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Everyone sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Everyone sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative Everyone sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Negative Everyone sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative Everyone sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
  }

  test("Negative Everyone sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Everyone sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive Exactly Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
  }

  test("Positive Exactly Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
  }

  test("Positive LessOrEqual Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), Everyone, None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative Exactly Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative LessOrEqual Number 0 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative LessOrEqual Number 0 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(0, LessOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (Some(false))
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
    sentence.compareWithTruth(truth4) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
  }

  test("Positive MoreOrEqual Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive MoreOrEqual Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )
    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative Exactly Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (None)
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative MoreOrEqual Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
  }

  test("Negative LessOrEqual Number 1 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative LessOrEqual Number 1 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(1, LessOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char2"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)

  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive Exactly Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive Exactly Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive MoreOrEqual Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive MoreOrEqual Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (None)
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth1) should be (Some(true))
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Positive LessOrEqual Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2"))),
        Character(Name("char5"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (Some(false))
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative Exactly Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative Exactly Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, Exactly), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (None)
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2"))),
        Character(Name("char5"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
  }

  test("Negative MoreOrEqual Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(false))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative MoreOrEqual Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, MoreOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race1")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), None),
        Character(Name("char3"), None),
        Character(Name("char4"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(true))
    sentence.compareWithTruth(truth4) should be (None)
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters some match some not, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters some match some not, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race1"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), None)
      )

    val truth4: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2"))),
        Character(Name("char5"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
    sentence.compareWithTruth(truth4) should be (Some(false))
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters not match, no undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2"))),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where defined characters not match, some undefined"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2"))),
        Character(Name("char3"), Some(TestRace("Race2"))),
        Character(Name("char4"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (None)
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative LessOrEqual Number 2 sentence compared with Truths where no defined characters"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (Some(true))
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (None)
  }

  test("Negative LessOrEqual Number 2 sentence comparing WorldState"){
    val sentence = Sentence(Name("char1"), NumberOfPeople(2, LessOrEqual), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None),
        Character(Name("char3"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
  }

  test("Positive sentence speaking about a Character"){
    val sentence = Sentence(Name("char1"), Name("char2"), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative sentence speaking about a Character"){
    val sentence = Sentence(Name("char1"), Name("char2"), None, StateDirectObject(TestRace("Race1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race1")))
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None),
        Character(Name("char2"), Some(TestRace("Race2")))
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }

  test("Positive sentence speaking about a WorldState"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = true)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState2"))),
        Character(Name("char1"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(true))
    sentence.compareWithTruth(truth3) should be (Some(false))
  }

  test("Negative sentence speaking about a WorldState"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), None, StateDirectObject(TestWorldState("worldState1")), directObjectAffirmation = false)

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truth3: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState2"))),
        Character(Name("char1"), None)
      )

    sentence.compareWithTruth(truth1) should be (None)
    sentence.compareWithTruth(truth2) should be (Some(false))
    sentence.compareWithTruth(truth3) should be (Some(true))
  }
}
