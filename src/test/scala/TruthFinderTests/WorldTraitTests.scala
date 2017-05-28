package TruthFinderTests

import org.scalatest.{FunSuite, Matchers}
import TestWorld._
import TruthEngine.Language._

class WorldTraitTests extends FunSuite with Matchers {
  test("TestWorld should return possible world aspects"){
    TestWorld.possibleWorldAspects(Some(TestWorldState("worldStateRef1State1"))) should be (List(TestWorldStateRef("worldStateRef1")))
    TestWorld.possibleWorldAspects(Some(TestWorldState("worldStateRef1State2"))) should be (List(TestWorldStateRef("worldStateRef1")))
    TestWorld.possibleWorldAspects(Some(TestWorldState("worldStateRef2State1"))) should be (List(TestWorldStateRef("worldStateRef2")))
    TestWorld.possibleWorldAspects(Some(TestWorldState("worldStateRef2State2"))) should be (List(TestWorldStateRef("worldStateRef2")))
    TestWorld.possibleWorldAspects(None) should be (List(TestWorldStateRef("worldStateRef1"), TestWorldStateRef("worldStateRef2")))
  }

  test("TestWorld should return possible world states"){
    TestWorld.possibleWorldStates(Some(TestWorldStateRef("worldStateRef1"))) should be (List(TestWorldState("worldStateRef1State1"), TestWorldState("worldStateRef1State2")))
    TestWorld.possibleWorldStates(Some(TestWorldStateRef("worldStateRef2"))) should be (List(TestWorldState("worldStateRef2State1"), TestWorldState("worldStateRef2State2")))
    TestWorld.possibleWorldStates(None) should be (List(TestWorldState("worldStateRef1State1"), TestWorldState("worldStateRef1State2"), TestWorldState("worldStateRef2State1"), TestWorldState("worldStateRef2State2")))
  }

  test("TestWorld should check consistency of a Truth"){
    val truth1 = List(Character(Name("A"), None))
    val truth2 = List(Character(Name("A"), Some(TestRace("Race1"))))
    val truth3 = List(Character(Name("A"), Some(TestRace("Race1"))), Character(Name("B"), Some(TestRace("Race1"))))
    val truth4 = List(WorldAspect(TestWorldStateRef("worldStateRef1"), None))
    val truth5 = List(WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1"))))
    val truth6 = List(WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State2"))))
    val truth7 = List()

    TestWorld.checkConsistency(truth1) should be (true)
    TestWorld.checkConsistency(truth2) should be (true)
    TestWorld.checkConsistency(truth3) should be (false)
    TestWorld.checkConsistency(truth4) should be (true)
    TestWorld.checkConsistency(truth5) should be (true)
    TestWorld.checkConsistency(truth6) should be (false)
    TestWorld.checkConsistency(truth7) should be (true)
  }

  test("TestWorld should return extra deductions from a Truth"){
    val truths1 = List(List(Character(Name("A"), Some(TestSubRace(Some(true))))))
    val truths2 = List(List(Character(Name("A"), Some(TestSubRace(Some(true))))), List(Character(Name("A"), Some(TestSubRace(Some(false))))))
    val truths3 = List(List(Character(Name("A"), Some(TestSubRace(Some(true))))), List(Character(Name("A"), Some(TestRace("NotSubRace")))))
    val truths4 = List(List(Character(Name("A"), Some(TestRace("NotSubRace")))))

    TestWorld.extraDeductions(truths1) should be (List("A is of Race TestSubRace"))
    TestWorld.extraDeductions(truths2) should be (List("A is of Race TestSubRace"))
    TestWorld.extraDeductions(truths3) should be (List())
    TestWorld.extraDeductions(truths4) should be (List())
  }
}
