package TruthFinderTests

import TruthEngine.Language._
import TruthEngine.Truth.Truth
import TruthEngine._


object TestWorld extends TestWorld

trait TestWorld extends World[TestWorld] {
  val worldInstance:TestWorld = TestWorld
  val name: String = "TestWorld"
  val description:String = "World for testing."

  def possibleWorldStates(war:Option[WorldAspectReference[TestWorld, WorldState[TestWorld]]]):List[WorldState[TestWorld]] =
    war match {
      case Some(TestWorldStateRef("worldStateRef1")) =>
        List(TestWorldState("worldStateRef1State1"), TestWorldState("worldStateRef1State2"))
      case Some(TestWorldStateRef("worldStateRef2")) =>
        List(TestWorldState("worldStateRef2State1"), TestWorldState("worldStateRef2State2"))
      case _ =>
        List(TestWorldState("worldStateRef1State1"), TestWorldState("worldStateRef1State2"), TestWorldState("worldStateRef2State1"), TestWorldState("worldStateRef2State2"))
    }

  def possibleWorldAspects(ws:Option[WorldState[TestWorld]]):List[WorldAspectReference[TestWorld, WorldState[TestWorld]]] =
    ws match {
      case Some(TestWorldState("worldStateRef1State1" | "worldStateRef1State2")) =>
        List(TestWorldStateRef("worldStateRef1"))
      case Some(TestWorldState("worldStateRef2State1" | "worldStateRef2State2")) =>
        List(TestWorldStateRef("worldStateRef2"))
      case _ =>
        List(TestWorldStateRef("worldStateRef1"), TestWorldStateRef("worldStateRef2"))
    }

  val races:List[Race] = List(
    TestRace("Race1"),
    TestRace("Race2"),
    TestTruthRace("Race1"),
    TestTruthRace("Race2"),
    TestLiarRace("Race1"),
    TestLiarRace("Race2"),
    TestSubRace(Some(true)),
    TestSubRace(Some(false)),
    TestSubRace(None)
  )

  //At most there is only 1 TestRace("Race1")
  //TestWorldStateRef("worldStateRef1") state is either undefined or TestWorldState("worldStateRef1State1")
  def checkConsistency(truth: Truth):Boolean =
    truth.count(_.state.contains(TestRace("Race1"))) < 2 &&
      truth.find(_.reference == TestWorldStateRef("worldStateRef1")).fold(true)(_.state.fold(true)(_ == TestWorldState("worldStateRef1State1")))


  override def extraDeductions(truths: List[Truth]): List[String] =
    traverseTruthResults(truths).collect {
      case (Name(charName), possibleStates) if !possibleStates.exists(s => s != TestSubRace(Some(true)) && s != TestSubRace(Some(false)) && s != TestSubRace(None)) =>
        s"$charName is of Race TestSubRace"
    }

  case class TestWorldState(stringRefPrefix:String) extends WorldState[TestWorld] {
    def stringRef:String = s"$stringRefPrefix: A world state for tests"
    def description:String = "A world state for testing"
  }

  case class TestWorldStateRef(ref:String) extends WorldAspectReference[TestWorld, TestWorldState]

  case class TestRace(stringRefPrefix:String) extends Race {
    def stringRef:String = s"$stringRefPrefix: TestRace"
    def description:String = "A race for tests"
    def personality(truthPieces: List[TruthPiece[State]], text:List[Sentence], sentenceIndex:Int): Boolean => Boolean = _ => true
  }

  case class TestTruthRace(stringRefPrefix:String) extends Race {
    def stringRef:String = s"$stringRefPrefix: TestTruthRace"
    def description:String = "A race for tests that speaks the truth"
    def personality(truthPieces: List[TruthPiece[State]], text:List[Sentence], sentenceIndex:Int): Boolean => Boolean = b => b
  }

  case class TestLiarRace(stringRefPrefix:String) extends Race {
    def stringRef:String = s"$stringRefPrefix: TestLiarRace"
    def description:String = "A race for tests that lies"
    def personality(truthPieces: List[TruthPiece[State]], text:List[Sentence], sentenceIndex:Int): Boolean => Boolean = b => !b
  }

  case class TestSubRace(subRace:Option[Boolean]) extends Race {
    def stringRef:String = "Test race with sub-races true and false"
    def description:String = "A race for tests"
    def personality(truthPieces: List[TruthPiece[State]], text:List[Sentence], sentenceIndex:Int): Boolean => Boolean = b => !b
  }
}