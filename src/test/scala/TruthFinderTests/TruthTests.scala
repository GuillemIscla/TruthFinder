package TruthFinderTests

import TruthEngine.Language._
import TruthEngine.Truth
import TruthEngine.Truth.Truth
import TruthFinderTests.TestWorld._
import org.scalatest.{FunSuite, Matchers}

class TruthTests extends FunSuite with Matchers {
  test("Two similar truths should merge into similar truth"){
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    Truth.merge(truth1, truth2, {case (_, _) => None}) should be (truth1)
  }

  test("Two not similar truths should merge into None truth"){
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race2")))
      )

    val truthResult: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    Truth.merge(truth1, truth2, {case (_, _) => None}) should be (truthResult)
  }

  test("Two truths with None should merge into None truth"){
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    val truthResult: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldState1"))),
        Character(Name("char1"), None)
      )

    Truth.merge(truth1, truth2, {case (_, _) => None}) should be (truthResult)
    Truth.merge(truth2, truth2, {case (_, _) => None}) should be (truthResult)
  }

  test("Next assumptions for WorldState should be all WorldStates"){
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), None)
      )

    val truthResult_1_1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), None)
      )

    val truthResult_1_2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))),
        Character(Name("char1"), None)
      )

    val truth2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef2"), None),
        Character(Name("char1"), None)
      )

    val truthResult_2_1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef2"), Some(TestWorldState("worldStateRef2State1"))),
        Character(Name("char1"), None)
      )

    val truthResult_2_2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef2"), Some(TestWorldState("worldStateRef2State2"))),
        Character(Name("char1"), None)
      )


    Truth.nextAssumptions(TestWorld, truth1) should be (List(truthResult_1_1, truthResult_1_2))

    Truth.nextAssumptions(TestWorld, truth2) should be (List(truthResult_2_1, truthResult_2_2))
  }

  test("Next assumptions for Character should be all Races"){
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), None)
      )

    val resultList:List[Truth] = TestWorld.races.map( race =>
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), Some(race))
      )
    )

    Truth.nextAssumptions(TestWorld, truth1) should be (resultList)
  }

  test("Truth tellers can speak true sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), Some(TestTruthRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("Truth tellers cannot speak false sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))),
        Character(Name("char1"), Some(TestTruthRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (false)
  }

  test("Truth tellers can speak undefined sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), Some(TestTruthRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("Liars cannot speak true sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), Some(TestLiarRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (false)
  }

  test("Liars can speak false sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))),
        Character(Name("char1"), Some(TestLiarRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("Liars can speak undefined sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), Some(TestLiarRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No personality characters can speak true sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No personality characters can speak false sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No personality characters can speak undefined sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), Some(TestRace("Race1")))
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No defined characters can speak true sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        Character(Name("char1"), None)
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No defined characters can speak false sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))),
        Character(Name("char1"), None)
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("No defined characters can speak undefined sentence"){
    val sentence = Sentence(Name("char1"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        Character(Name("char1"), None)
      )

    Truth.sentenceCanBeSpoken(TestWorld, truth1, List(sentence), 0) should be (true)
  }

  test("Infer conversation with 0 truth"){
    val conversation = List(
      Sentence(Name("char1"), Name("char2"), TestTruthRace("Race1"), directObjectAffirmation = true),
      Sentence(Name("char2"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true),
      Sentence(Name("char2"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = false)
    )

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        WorldAspect(TestWorldStateRef("worldStateRef2"), None),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), None)
      )

    Truth.compareTextAndTruth(TestWorld, truth1, conversation) should be (List())
  }

  test("Infer conversation with 1 truth"){
    val conversation = List(
      Sentence(Name("char1"), Name("char2"), TestTruthRace("Race1"), directObjectAffirmation = true),
      Sentence(Name("char2"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true),
      Sentence(Name("char2"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), directObjectAffirmation = true)
    )

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        WorldAspect(TestWorldStateRef("worldStateRef2"), None),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truthResult: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        WorldAspect(TestWorldStateRef("worldStateRef2"), Some(TestWorldState("worldStateRef2State1"))),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), Some(TestTruthRace("Race1")))
      )

    Truth.compareTextAndTruth(TestWorld, truth1, conversation) should be (List(truthResult))
  }

  test("Infer conversation with multiple truths"){
    val conversation = List(
      Sentence(Name("char1"), Name("char2"), TestTruthRace("Race1"), directObjectAffirmation = true),
      Sentence(Name("char2"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    )

    val truth1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), None),
        WorldAspect(TestWorldStateRef("worldStateRef2"), None),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), None)
      )

    val truthResult1: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        WorldAspect(TestWorldStateRef("worldStateRef2"), Some(TestWorldState("worldStateRef2State1"))),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), Some(TestTruthRace("Race1")))
      )

    val truthResult2: Truth =
      List(
        WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))),
        WorldAspect(TestWorldStateRef("worldStateRef2"), Some(TestWorldState("worldStateRef2State2"))),
        Character(Name("char1"), Some(TestTruthRace("Race1"))),
        Character(Name("char2"), Some(TestTruthRace("Race1")))
      )

    Truth.compareTextAndTruth(TestWorld, truth1, conversation) should be (List(truthResult1, truthResult2))
  }
}
