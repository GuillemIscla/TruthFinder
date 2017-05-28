package TruthFinderTests

import TruthEngine.Language._
import TruthEngine.ParserHelper.{GenericParser, RegularWorldStateParser}
import TruthEngine._
import TruthFinderTests.TestWorld._
import org.scalatest.{FunSuite, Matchers}

trait TranslatorTests extends FunSuite with Matchers{

  case class SentenceTranslator(index:Int) extends Translator[String, Int] {
    def translate(script:String):Translation[String, Int] = {
      val preTranslatedRegex = s"(Translate$index) (\\d+)"
      val translatedRegex = preTranslatedRegex.r
      val preErrorRegex = s"(SentenceTranslator${index}Error)"
      val errorRegex = preErrorRegex.r
      script match {
        case translatedRegex(_, number) =>
          Translated(number.toInt)
        case errorRegex(_) =>
          TranslationError(script, s"SentenceTranslator with index $index thought it could parse this script but it could not")
        case _ =>
          NotTranslated(script)
      }
    }
  }

  case object TestParser extends LineParser[TestWorld] {
    val world:TestWorld = TestWorld
    val forbiddenNames: List[String] = List("customcopulativeverbref1", "customcopulativeverbref2")
    val parserName:String = "TestParser"

    def translate(script:String):Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): It CustomCopulativeVerbRef(\d+)( not|) (.+)""".r
      script match {
        case sentenceRegex(raw_speaker, raw_ref_number, raw_maybe_not, raw_world_state) =>
          val speaker = Name(raw_speaker)
          val maybeNot = raw_maybe_not != " not"
          (raw_ref_number, raw_world_state) match{
            case ("1", "worldStateRef1State1: A world state for tests") =>
              Translated(Sentence(speaker, TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), maybeNot))
            case ("1", "worldStateRef1State2: A world state for tests") =>
              Translated(Sentence(speaker, TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State2"), maybeNot))
            case ("2", "worldStateRef2State1: A world state for tests") =>
              Translated(Sentence(speaker, TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), maybeNot))
            case ("2", "worldStateRef2State2: A world state for tests") =>
              Translated(Sentence(speaker, TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State2"), maybeNot))
            case _ =>
              TranslationError(script, "TestParser thought it could translate it but it could not")
          }
        case _ =>
          NotTranslated(script)
      }
    }
  }

  case object TestPrinter extends TruthPiecePrinter {
    def translate(script: TruthPiece[State]): Translation[TruthPiece[State], String] =
      (script.reference, script.state) match {
        case (TestWorldStateRef("CustomAspect"), Some(ws)) =>
          Translated(s"It CustomCopulative ${ws.stringRef}")
        case (Name(charName), Some(TestRace("CustomRace"))) =>
          Translated(s"$charName is a member of the customized Race")
        case _ =>
          NotTranslated(script)
      }
  }
}

class GeneralTranslatorTests extends TranslatorTests {
  test("Test translator should translate translatable sentences"){
    val translator = SentenceTranslator(1)
    val sentence = "Translate1 3"

    translator.translate(sentence) should be (Translated(3))
  }

  test("Test translator should go to error on error sentences"){
    val translator = SentenceTranslator(1)
    val sentence = "SentenceTranslator1Error"

    translator.translate(sentence) should be (TranslationError("SentenceTranslator1Error", s"SentenceTranslator with index 1 thought it could parse this script but it could not"))
  }

  test("Test translator should not translate non translatable sentences"){
    val translator = SentenceTranslator(1)
    val sentence = "Not translatable"

    translator.translate(sentence) should be (NotTranslated(sentence))
  }
}

class ParserTests extends TranslatorTests {
  test("TestParser should translate custom copulative verb sentence"){

    TestParser.translate("A: It CustomCopulativeVerbRef1 worldStateRef1State1: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)))
    TestParser.translate("A: It CustomCopulativeVerbRef1 worldStateRef1State2: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State2"), directObjectAffirmation = true)))
    TestParser.translate("A: It CustomCopulativeVerbRef2 worldStateRef2State1: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), directObjectAffirmation = true)))
    TestParser.translate("A: It CustomCopulativeVerbRef2 worldStateRef2State2: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State2"), directObjectAffirmation = true)))

    TestParser.translate("A: It CustomCopulativeVerbRef1 not worldStateRef1State1: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = false)))
    TestParser.translate("A: It CustomCopulativeVerbRef1 not worldStateRef1State2: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State2"), directObjectAffirmation = false)))
    TestParser.translate("A: It CustomCopulativeVerbRef2 not worldStateRef2State1: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), directObjectAffirmation = false)))
    TestParser.translate("A: It CustomCopulativeVerbRef2 not worldStateRef2State2: A world state for tests") should be (Translated(Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State2"), directObjectAffirmation = false)))
  }

  test("TestParser should go to error on wrong custom copulative verb sentence"){
    val sentence1 = "A: It CustomCopulativeVerbRef1 worldStateRef2State1"
    val sentence2 = "A: It CustomCopulativeVerbRef2 worldStateRef1State1"
    val sentence3 = "A: It CustomCopulativeVerbRef3 worldStateRef3State1"

    val testParserError = "TestParser thought it could translate it but it could not"

    TestParser.translate(sentence1) should be (TranslationError(sentence1, testParserError))
    TestParser.translate(sentence2) should be (TranslationError(sentence2, testParserError))
    TestParser.translate(sentence3) should be (TranslationError(sentence3, testParserError))
  }

  test("TestParser should not translate other sentence"){
    val sentence = "A: This sentence will be not translated"

    TestParser.translate(sentence) should be (NotTranslated(sentence))
  }
}

class CommonParserTests extends TranslatorTests {
  test("GenericParser should translate I am sentences"){
    val sentenceScript1 = "A: I am Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Name("A"), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: I am not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Name("A"), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }

  test("GenericParser should translate other char is sentences"){
    val sentenceScript1 = "A: B is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Name("B"), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: B is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Name("B"), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }

  test("GenericParser should translate Someone is sentences"){
    val sentenceScript1 = "A: Someone is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: Someone is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }

  test("GenericParser should translate Everyone is sentences"){
    val sentenceScript1 = "A: Everyone is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Everyone, TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: Everyone is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Everyone, TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }

  test("GenericParser should translate No one is sentences"){
    val sentenceScript1 = "A: No one is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(0, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: No one is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(0, Exactly), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }

  test("GenericParser should translate There (is|are) at least sentences"){
    val sentenceScript1 = "A: There is at least 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is at least 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are at least 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are at least 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript3) should be (Translated(translatedSentence3))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript4) should be (Translated(translatedSentence4))
  }

  test("GenericParser should translate There (is|are) at most sentences"){
    val sentenceScript1 = "A: There is at most 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is at most 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are at most 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, LessOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are at most 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, LessOrEqual), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript3) should be (Translated(translatedSentence3))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript4) should be (Translated(translatedSentence4))
  }

  test("GenericParser should translate There (is|are) exactly sentences"){
    val sentenceScript1 = "A: There is exactly 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is exactly 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, Exactly), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are exactly 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are exactly 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, Exactly), TestRace("Race1"), directObjectAffirmation = false)

    GenericParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript3) should be (Translated(translatedSentence3))
    GenericParser(TestWorld.worldInstance).translate(sentenceScript4) should be (Translated(translatedSentence4))
  }

  test("RegularWorldStateParser should translate is sentences"){
    val sentenceScript1 = "A: It is worldStateRef1State1: A world state for tests"
    val translatedSentence1 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: It is not worldStateRef1State1: A world state for tests"
    val translatedSentence2 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = false)

    RegularWorldStateParser(TestWorld.worldInstance).translate(sentenceScript1) should be (Translated(translatedSentence1))
    RegularWorldStateParser(TestWorld.worldInstance).translate(sentenceScript2) should be (Translated(translatedSentence2))
  }
}

class TextParserTest extends TranslatorTests {
  val textParser = TextParser(TestWorld, List(TestParser, RegularWorldStateParser(TestWorld.worldInstance), GenericParser(TestWorld.worldInstance)))

  test("TextParserTest should translate I am sentences"){
    val sentenceScript1 = "A: I am Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Name("A"), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: I am not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Name("A"), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
  }

  test("TextParserTest should translate other char is sentences"){
    val sentenceScript1 = "A: B is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Name("B"), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: B is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Name("B"), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None),
      Character(Name("B"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
  }

  test("TextParserTest should translate Someone is sentences"){
    val sentenceScript1 = "A: Someone is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: Someone is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
  }

  test("TextParserTest should translate Everyone is sentences"){
    val sentenceScript1 = "A: Everyone is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), Everyone, TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: Everyone is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), Everyone, TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
  }

  test("TextParserTest should translate No one is is sentences"){
    val sentenceScript1 = "A: No one is Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(0, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: No one is not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(0, Exactly), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
  }

  test("TextParserTest should translate There (is|are) at least sentences"){
    val sentenceScript1 = "A: There is at least 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is at least 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are at least 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are at least 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, MoreOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
    textParser.translate(List(sentenceScript3)) should be (Translated((socratesTruth, List(translatedSentence3))))
    textParser.translate(List(sentenceScript4)) should be (Translated((socratesTruth, List(translatedSentence4))))
  }

  test("TextParserTest should translate There (is|are) at most sentences"){
    val sentenceScript1 = "A: There is at most 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is at most 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are at most 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, LessOrEqual), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are at most 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, LessOrEqual), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )
    val textParser = TextParser(TestWorld, List(GenericParser(TestWorld.worldInstance), RegularWorldStateParser(TestWorld.worldInstance), TestParser))

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
    textParser.translate(List(sentenceScript3)) should be (Translated((socratesTruth, List(translatedSentence3))))
    textParser.translate(List(sentenceScript4)) should be (Translated((socratesTruth, List(translatedSentence4))))
  }

  test("TextParserTest should translate There (is|are) exactly sentences"){
    val sentenceScript1 = "A: There is exactly 1 Race1: TestRace"
    val translatedSentence1 = Sentence(Name("A"), NumberOfPeople(1, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: There is exactly 1 not Race1: TestRace"
    val translatedSentence2 = Sentence(Name("A"), NumberOfPeople(1, Exactly), TestRace("Race1"), directObjectAffirmation = false)
    val sentenceScript3 = "A: There are exactly 2 Race1: TestRace"
    val translatedSentence3 = Sentence(Name("A"), NumberOfPeople(2, Exactly), TestRace("Race1"), directObjectAffirmation = true)
    val sentenceScript4 = "A: There are exactly 2 not Race1: TestRace"
    val translatedSentence4 = Sentence(Name("A"), NumberOfPeople(2, Exactly), TestRace("Race1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )
    val textParser = TextParser(TestWorld, List(GenericParser(TestWorld.worldInstance), RegularWorldStateParser(TestWorld.worldInstance), TestParser))

    textParser.translate(List(sentenceScript1)) should be (Translated((socratesTruth, List(translatedSentence1))))
    textParser.translate(List(sentenceScript2)) should be (Translated((socratesTruth, List(translatedSentence2))))
    textParser.translate(List(sentenceScript3)) should be (Translated((socratesTruth, List(translatedSentence3))))
    textParser.translate(List(sentenceScript4)) should be (Translated((socratesTruth, List(translatedSentence4))))
  }

  test("TextParserTest should translate is (WorldState) sentences"){
    val sentenceScript1 = "A: It is worldStateRef1State1: A world state for tests"
    val translatedSentence1 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: It is not worldStateRef1State1: A world state for tests"
    val translatedSentence2 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = false)
    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated(socratesTruth, List(translatedSentence1)))
    textParser.translate(List(sentenceScript2)) should be (Translated(socratesTruth, List(translatedSentence2)))
  }

  test("TextParserTest should translate custom copulative verb sentences"){
    val sentenceScript1 = "A: It CustomCopulativeVerbRef1 worldStateRef1State1: A world state for tests"
    val translatedSentence1 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: It CustomCopulativeVerbRef2 worldStateRef2State1: A world state for tests"
    val translatedSentence2 = Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), directObjectAffirmation = true)
    val sentenceScript3 = "A: It is worldStateRef1State1: A world state for tests"
    val translatedSentence3 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)

    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated(socratesTruth, List(translatedSentence1)))
    textParser.translate(List(sentenceScript2)) should be (Translated(socratesTruth, List(translatedSentence2)))
    textParser.translate(List(sentenceScript3)) should be (Translated(socratesTruth, List(translatedSentence3)))
  }

  test("TextParserTest should go to error if has not translated sentences"){
    val sentence = "A: This sentence will be not translated"

    textParser.translate(List(sentence)) should be (TranslationError(sentence, "Was not understood by the parsers"))
  }

  test("TextParserTest should go to error on wrong custom names"){
    val sentenceScript1 = "A: It CustomCopulativeVerbRef1 worldStateRef1State1: A world state for tests"
    val translatedSentence1 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)
    val sentenceScript2 = "A: It CustomCopulativeVerbRef2 worldStateRef2State1: A world state for tests"
    val translatedSentence2 = Sentence(Name("A"), TestWorldStateRef("worldStateRef2"), TestWorldState("worldStateRef2State1"), directObjectAffirmation = true)
    val sentenceScript3 = "A: It is worldStateRef1State1: A world state for tests"
    val translatedSentence3 = Sentence(Name("A"), TestWorldStateRef("worldStateRef1"), TestWorldState("worldStateRef1State1"), directObjectAffirmation = true)

    val socratesTruth = List(
      WorldAspect(TestWorldStateRef("worldStateRef1"), None),
      WorldAspect(TestWorldStateRef("worldStateRef2"), None),
      Character(Name("A"), None)
    )

    textParser.translate(List(sentenceScript1)) should be (Translated(socratesTruth, List(translatedSentence1)))
    textParser.translate(List(sentenceScript2)) should be (Translated(socratesTruth, List(translatedSentence2)))
    textParser.translate(List(sentenceScript3)) should be (Translated(socratesTruth, List(translatedSentence3)))
  }

  test("TextParserTest should go to error on wrong names"){
    val sentence1 = "CustomCopulativeVerbRef1: I am Race1: TestRace"
    val sentence2 = "CustomCopulativeVerbRef2: I am Race1: TestRace"

    textParser.translate(List(sentence1)) should be (TranslationError.invalidCharacterError("CustomCopulativeVerbRef1"))
    textParser.translate(List(sentence2)) should be (TranslationError.invalidCharacterError("CustomCopulativeVerbRef2"))
  }
}

class CustomPrinterTests extends TranslatorTests {
  test("CustomPrinter should print custom printable Race TruthPieces"){
    val char = Character(Name("A"), Some(TestRace("CustomRace")))

    TestPrinter.translate(char) should be (Translated("A is a member of the customized Race"))
  }

  test("CustomPrinter should print custom printable WorldState TruthPieces"){
    val was = WorldAspect(TestWorldStateRef("CustomAspect"), Some(TestWorldState("worldStateRef1State1")))

    TestPrinter.translate(was) should be (Translated("It CustomCopulative worldStateRef1State1: A world state for tests"))
  }
}

class FinalPrinterTests extends TranslatorTests {
  test("FinalPrinter should print Race TruthPieces"){
    val char = Character(Name("A"), Some(TestRace("Race1")))

    PrinterHelper.FinalPrinter.translate(char) should be (Translated("A is Race1: TestRace"))
  }

  test("FinalPrinter should print WorldState TruthPieces"){
    val was = WorldAspect(TestWorldStateRef("worldStateRef1"), Some(TestWorldState("worldStateRef1State1")))

    PrinterHelper.FinalPrinter.translate(was) should be (Translated("It is worldStateRef1State1: A world state for tests"))
  }
}