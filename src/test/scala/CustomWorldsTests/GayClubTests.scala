package CustomWorldsTests

import TruthEngine.Language._
import TruthEngine.ParserHelper.{GenericParser, RegularWorldStateParser}
import TruthEngine.Truth.Truth
import TruthEngine.{TextParser, Translated, TranslationError, Truth}
import Worlds.GayClub
import Worlds.GayClub._


trait GayClubTests extends CustomWorldTests[GayClub] {
  def world: GayClub = GayClub

  def gayClubParser = TextParser(worldInstance, customParsers ++ List(RegularWorldStateParser(worldInstance), GenericParser(worldInstance)))

  def checkNonCopulative(sentence:Sentence, truth:Truth, truthPieceIndex:Int):Option[Boolean] =
    sentence.maybeNonCopulativeVerb.flatMap(_.truthPieceTrue(sentence, truth, truthPieceIndex))
}

class GayClubCustomParsingTests extends GayClubTests {
  test("Custom parsing I like Songs in GayClub") {
    val sentenceScript1 = "A: I like Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: I don't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing other people like Songs in GayClub") {
    val sentenceScript1 = "A: B likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: B doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), Name("B"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), Name("B"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing Someone likes Songs in GayClub") {
    val sentenceScript1 = "A: Someone likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: Someone doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing No one likes Songs in GayClub") {
    val sentenceScript1 = "A: No one likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: No one doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing There is at least people who likes Songs in GayClub") {
    val sentenceScript1 = "A: There is at least 1 who likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: There is at least 1 who doesn't like Sweet Dreams by Eurythmics"
    val sentenceScript3 = "A: There are at least 5 who likes Sweet Dreams by Eurythmics"
    val sentenceScript4 = "A: There are at least 5 who doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)


    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is at most people who likes Songs in GayClub") {
    val sentenceScript1 = "A: There is at most 1 who likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: There is at most 1 who doesn't like Sweet Dreams by Eurythmics"
    val sentenceScript3 = "A: There are at most 5 who likes Sweet Dreams by Eurythmics"
    val sentenceScript4 = "A: There are at most 5 who doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is exactly people who likes Songs in GayClub") {
    val sentenceScript1 = "A: There is exactly 1 who likes Sweet Dreams by Eurythmics"
    val sentenceScript2 = "A: There is exactly 1 who doesn't like Sweet Dreams by Eurythmics"
    val sentenceScript3 = "A: There are exactly 5 who likes Sweet Dreams by Eurythmics"
    val sentenceScript4 = "A: There are exactly 5 who doesn't like Sweet Dreams by Eurythmics"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }


  test("Custom parsing I like people in GayClub"){
    val sentenceScript1 = "A: I like B"
    val sentenceScript2 = "A: I don't like B"
    val sentence1 = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing Someone likes people in GayClub"){
    val sentenceScript1 = "A: Someone likes B"
    val sentenceScript2 = "A: Someone doesn't like B"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing No one likes people in GayClub"){
    val sentenceScript1 = "A: No one likes B"
    val sentenceScript2 = "A: No one doesn't like B"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing There is at least who likes people in GayClub"){
    val sentenceScript1 = "A: There is at least 1 who likes B"
    val sentenceScript2 = "A: There is at least 1 who doesn't like B"
    val sentenceScript3 = "A: There are at least 5 who likes B"
    val sentenceScript4 = "A: There are at least 5 who doesn't like B"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is at most who likes people in GayClub"){
    val sentenceScript1 = "A: There is at most 1 who likes B"
    val sentenceScript2 = "A: There is at most 1 who doesn't like B"
    val sentenceScript3 = "A: There are at most 5 who likes B"
    val sentenceScript4 = "A: There are at most 5 who doesn't like B"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is exactly who likes people in GayClub"){
    val sentenceScript1 = "A: There is exactly 1 who likes B"
    val sentenceScript2 = "A: There is exactly 1 who doesn't like B"
    val sentenceScript3 = "A: There are exactly 5 who likes B"
    val sentenceScript4 = "A: There are exactly 5 who doesn't like B"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }


  test("Custom parsing I like gender in GayClub"){
    val sentenceScript1 = "A: I like girls"
    val sentenceScript2 = "A: I don't like boys"
    val sentence1 = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing Someone likes gender in GayClub"){
    val sentenceScript1 = "A: Someone likes girls"
    val sentenceScript2 = "A: Someone doesn't like boys"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing No one likes gender in GayClub"){
    val sentenceScript1 = "A: No one likes girls"
    val sentenceScript2 = "A: No one doesn't like boys"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(0, Exactly), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
  }

  test("Custom parsing There is at least who likes gender in GayClub"){
    val sentenceScript1 = "A: There is at least 1 who likes girls"
    val sentenceScript2 = "A: There is at least 1 who doesn't like girls"
    val sentenceScript3 = "A: There are at least 5 who likes boys"
    val sentenceScript4 = "A: There are at least 5 who doesn't like boys"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, MoreOrEqual), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is at most who likes gender in GayClub"){
    val sentenceScript1 = "A: There is at most 1 who likes girls"
    val sentenceScript2 = "A: There is at most 1 who doesn't like girls"
    val sentenceScript3 = "A: There are at most 5 who likes boys"
    val sentenceScript4 = "A: There are at most 5 who doesn't like boys"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, LessOrEqual), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, LessOrEqual), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing There is exactly who likes gender in GayClub"){
    val sentenceScript1 = "A: There is exactly 1 who likes girls"
    val sentenceScript2 = "A: There is exactly 1 who doesn't like girls"
    val sentenceScript3 = "A: There are exactly 5 who likes boys"
    val sentenceScript4 = "A: There are exactly 5 who doesn't like boys"
    val sentence1 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), NumberOfPeople(1, Exactly), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val sentence3 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), NumberOfPeople(5, Exactly), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
  }

  test("Custom parsing DJ Someone like sentences"){
    val sentenceScript1 = "BigMomDJ: Someone likes this song"
    val sentenceScript2 = "BigMomDJ: Someone likes next song"
    val sentenceScript3 = "BigMomDJ: Someone likes the song in 2 songs"
    val sentenceScript4 = "BigMomDJ: Someone doesn't like this song"
    val sentenceScript5 = "BigMomDJ: Someone doesn't like next song"
    val sentenceScript6 = "BigMomDJ: Someone doesn't like the song in 2 songs"


    val sentence1 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val sentence3 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(2)), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = false)
    val sentence5 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = false)
    val sentence6 = Sentence(Name("BigMomDJ"), NumberOfPeople(1, MoreOrEqual), Some(Like), ReferenceDirectObject(SongReference(2)), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
    gayClubParser.translate(List(sentenceScript5)).map(_._2) should be (Translated(List(sentence5)))
    gayClubParser.translate(List(sentenceScript6)).map(_._2) should be (Translated(List(sentence6)))
  }

  test("Custom parsing about the playlist"){
    val sentenceScript1 = "BigMomDJ: This song is Sweet Dreams by Eurythmics"
    val sentenceScript2 = "BigMomDJ: Next song is Sweet Dreams by Eurythmics"
    val sentenceScript3 = "BigMomDJ: The song in 1 songs is Sweet Dreams by Eurythmics"
    val sentenceScript4 = "BigMomDJ: The song in 2 songs is Sweet Dreams by Eurythmics"
    val sentenceScript5 = "BigMomDJ: This song is not Sweet Dreams by Eurythmics"
    val sentenceScript6 = "BigMomDJ: Next song is not Sweet Dreams by Eurythmics"
    val sentenceScript7 = "BigMomDJ: The song in 1 songs is not Sweet Dreams by Eurythmics"
    val sentenceScript8 = "BigMomDJ: The song in 2 songs is not Sweet Dreams by Eurythmics"


    val sentence1 = Sentence(Name("BigMomDJ"), SongReference(0), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("BigMomDJ"), SongReference(1), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("BigMomDJ"), SongReference(2), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    val sentence5 = Sentence(Name("BigMomDJ"), SongReference(0), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)
    val sentence6 = Sentence(Name("BigMomDJ"), SongReference(1), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)
    val sentence8 = Sentence(Name("BigMomDJ"), SongReference(2), None, StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = false)

    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (TranslationError("The song in 1 songs", """GayClubDJParser claims that the pattern The song in \d+ songs, should contain a number > 1"""))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
    gayClubParser.translate(List(sentenceScript5)).map(_._2) should be (Translated(List(sentence5)))
    gayClubParser.translate(List(sentenceScript6)).map(_._2) should be (Translated(List(sentence6)))
    gayClubParser.translate(List(sentenceScript7)).map(_._2) should be (TranslationError("The song in 1 songs", """GayClubDJParser claims that the pattern The song in \d+ songs, should contain a number > 1"""))
    gayClubParser.translate(List(sentenceScript8)).map(_._2) should be (Translated(List(sentence8)))
  }

  test("Custom parsing person"){
    val sentenceScript1 = "A: I am a boy"
    val sentenceScript2 = "A: I am a girl"
    val sentenceScript3 = "A: I am heterosexual"
    val sentenceScript4 = "A: I am gay"
    val sentenceScript5 = "A: I am bisexual"
    val sentenceScript6 = "A: I am asexual"
    val sentenceScript7 = "A: I am drunk"
    val sentenceScript8 = "A: I am sober"
    val sentenceScript9 = "A: I am a jerk"
    val sentenceScript10 = "A: I am DJ"
    val sentenceScript11 = "A: I am a waiter"
    val sentenceScript12 = "A: I am not a boy"
    val sentenceScript13 = "A: I am not a girl"
    val sentenceScript14 = "A: I am not heterosexual"
    val sentenceScript15 = "A: I am not gay"
    val sentenceScript16 = "A: I am not bisexual"
    val sentenceScript17 = "A: I am not asexual"
    val sentenceScript18 = "A: I am not drunk"
    val sentenceScript19 = "A: I am not sober"
    val sentenceScript20 = "A: I am not a jerk"
    val sentenceScript21 = "A: I am not DJ"
    val sentenceScript22 = "A: I am not a waiter"
    val sentenceScript23 = "A: B is a boy"
    val sentenceScript24 = "A: Everyone is a boy"
    val sentenceScript25 = "A: Someone is a boy"
    val sentenceScript26 = "A: No one is a boy"
    val sentenceScript27 = "A: There are exactly 3 boys"
    val sentenceScript28 = "A: There are at least 3 boys"
    val sentenceScript29 = "A: There are at most 3 boys"


    val sentence1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Girl), None, None, None, None)), directObjectAffirmation = true)
    val sentence3 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Heterosexual), None, None, None)), directObjectAffirmation = true)
    val sentence4 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Gay), None, None, None)), directObjectAffirmation = true)
    val sentence5 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Bisexual), None, None, None)), directObjectAffirmation = true)
    val sentence6 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Asexual), None, None, None)), directObjectAffirmation = true)
    val sentence7 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Drunk), None, None)), directObjectAffirmation = true)
    val sentence8 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Sober), None, None)), directObjectAffirmation = true)
    val sentence9 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Jerk), None, None)), directObjectAffirmation = true)
    val sentence10 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Worker(Some(DJ))), directObjectAffirmation = true)
    val sentence11 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Worker(Some(Waiter))), directObjectAffirmation = true)
    val sentence12 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = false)
    val sentence13 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Girl), None, None, None, None)), directObjectAffirmation = false)
    val sentence14 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Heterosexual), None, None, None)), directObjectAffirmation = false)
    val sentence15 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Gay), None, None, None)), directObjectAffirmation = false)
    val sentence16 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Bisexual), None, None, None)), directObjectAffirmation = false)
    val sentence17 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Asexual), None, None, None)), directObjectAffirmation = false)
    val sentence18 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Drunk), None, None)), directObjectAffirmation = false)
    val sentence19 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Sober), None, None)), directObjectAffirmation = false)
    val sentence20 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Jerk), None, None)), directObjectAffirmation = false)
    val sentence21 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Worker(Some(DJ))), directObjectAffirmation = false)
    val sentence22 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Worker(Some(Waiter))), directObjectAffirmation = false)
    val sentence23 = Sentence(Name("A"), Name("B"), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence24 = Sentence(Name("A"), Everyone, None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence25 = Sentence(Name("A"), NumberOfPeople(1, MoreOrEqual), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence26 = Sentence(Name("A"), NumberOfPeople(0, Exactly), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence27 = Sentence(Name("A"), NumberOfPeople(3, Exactly), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence28 = Sentence(Name("A"), NumberOfPeople(3, MoreOrEqual), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)
    val sentence29 = Sentence(Name("A"), NumberOfPeople(3, LessOrEqual), None, StateDirectObject(Customer(Some(Boy), None, None, None, None)), directObjectAffirmation = true)


    gayClubParser.translate(List(sentenceScript1)).map(_._2) should be (Translated(List(sentence1)))
    gayClubParser.translate(List(sentenceScript2)).map(_._2) should be (Translated(List(sentence2)))
    gayClubParser.translate(List(sentenceScript3)).map(_._2) should be (Translated(List(sentence3)))
    gayClubParser.translate(List(sentenceScript4)).map(_._2) should be (Translated(List(sentence4)))
    gayClubParser.translate(List(sentenceScript5)).map(_._2) should be (Translated(List(sentence5)))
    gayClubParser.translate(List(sentenceScript6)).map(_._2) should be (Translated(List(sentence6)))
    gayClubParser.translate(List(sentenceScript7)).map(_._2) should be (Translated(List(sentence7)))
    gayClubParser.translate(List(sentenceScript8)).map(_._2) should be (Translated(List(sentence8)))
    gayClubParser.translate(List(sentenceScript9)).map(_._2) should be (Translated(List(sentence9)))
    gayClubParser.translate(List(sentenceScript10)).map(_._2) should be (Translated(List(sentence10)))
    gayClubParser.translate(List(sentenceScript11)).map(_._2) should be (Translated(List(sentence11)))
    gayClubParser.translate(List(sentenceScript12)).map(_._2) should be (Translated(List(sentence12)))
    gayClubParser.translate(List(sentenceScript13)).map(_._2) should be (Translated(List(sentence13)))
    gayClubParser.translate(List(sentenceScript14)).map(_._2) should be (Translated(List(sentence14)))
    gayClubParser.translate(List(sentenceScript15)).map(_._2) should be (Translated(List(sentence15)))
    gayClubParser.translate(List(sentenceScript16)).map(_._2) should be (Translated(List(sentence16)))
    gayClubParser.translate(List(sentenceScript17)).map(_._2) should be (Translated(List(sentence17)))
    gayClubParser.translate(List(sentenceScript18)).map(_._2) should be (Translated(List(sentence18)))
    gayClubParser.translate(List(sentenceScript19)).map(_._2) should be (Translated(List(sentence19)))
    gayClubParser.translate(List(sentenceScript20)).map(_._2) should be (Translated(List(sentence20)))
    gayClubParser.translate(List(sentenceScript21)).map(_._2) should be (Translated(List(sentence21)))
    gayClubParser.translate(List(sentenceScript22)).map(_._2) should be (Translated(List(sentence22)))
    gayClubParser.translate(List(sentenceScript23)).map(_._2) should be (Translated(List(sentence23)))
    gayClubParser.translate(List(sentenceScript24)).map(_._2) should be (Translated(List(sentence24)))
    gayClubParser.translate(List(sentenceScript25)).map(_._2) should be (Translated(List(sentence25)))
    gayClubParser.translate(List(sentenceScript26)).map(_._2) should be (Translated(List(sentence26)))
    gayClubParser.translate(List(sentenceScript27)).map(_._2) should be (Translated(List(sentence27)))
    gayClubParser.translate(List(sentenceScript28)).map(_._2) should be (Translated(List(sentence28)))
    gayClubParser.translate(List(sentenceScript29)).map(_._2) should be (Translated(List(sentence29)))
  }
}

class GayClubCopulativeVerbsTests extends GayClubTests {
  test("Checking sentences with basic customer truth") {
    val sentenceGender = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Girl), None, None, None, None)), directObjectAffirmation = true)
    val sentenceSexualPreference = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Gay), None, None, None)), directObjectAffirmation = true)
    val sentenceMood = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Drunk), None, None)), directObjectAffirmation = true)
    val sentencePersonCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(None), None)), directObjectAffirmation = true)
    val sentencePersonCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(Some(Name("B"))), None)), directObjectAffirmation = true)
    val sentenceSongCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(None))), directObjectAffirmation = true)
    val sentenceSongCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))), directObjectAffirmation = true)

    val truthPieceNone = Character(Name("A"), Some(Customer(None, None, None, None, None)))

    sentenceGender.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentenceSexualPreference.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentenceMood.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentencePersonCrush1.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentencePersonCrush2.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentenceSongCrush1.compareWithTruth(List(truthPieceNone)) should be (Some(true))
    sentenceSongCrush2.compareWithTruth(List(truthPieceNone)) should be (Some(true))
  }

  test("Checking sentences with customer partial truth gender") {
    val sentenceGender = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Girl), None, None, None, None)), directObjectAffirmation = true)

    val truthPieceGenderTrue = Character(Name("A"), Some(Customer(Some(Girl), None, None, None, None)))
    val truthPieceGenderFalse = Character(Name("A"), Some(Customer(Some(Boy), None, None, None, None)))

    sentenceGender.compareWithTruth(List(truthPieceGenderTrue)) should be (Some(true))
    sentenceGender.compareWithTruth(List(truthPieceGenderFalse)) should be (Some(false))
  }

  test("Checking sentences with customer partial truth sexual preference") {
    val sentenceSexualPreference = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Gay), None, None, None)), directObjectAffirmation = true)

    val truthSexualPreferenceTrue = Character(Name("A"), Some(Customer(None, Some(Gay), None, None, None)))
    val truthSexualPreferenceFalse = Character(Name("A"), Some(Customer(None, Some(Heterosexual), None, None, None)))

    sentenceSexualPreference.compareWithTruth(List(truthSexualPreferenceTrue)) should be (Some(true))
    sentenceSexualPreference.compareWithTruth(List(truthSexualPreferenceFalse)) should be (Some(false))
  }

  test("Checking sentences with customer partial truth mood") {
    val sentenceMood = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Drunk), None, None)), directObjectAffirmation = true)

    val truthPieceMoodTrue = Character(Name("A"), Some(Customer(None, None, Some(Drunk), None, None)))
    val truthPieceMoodFalse = Character(Name("A"), Some(Customer(None, None, Some(Jerk), None, None)))

    sentenceMood.compareWithTruth(List(truthPieceMoodTrue)) should be (Some(true))
    sentenceMood.compareWithTruth(List(truthPieceMoodFalse)) should be (Some(false))
  }

  test("Checking sentences with customer partial truth person crush") {
    val sentencePersonCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(None), None)), directObjectAffirmation = true)
    val sentencePersonCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(Some(Name("B"))), None)), directObjectAffirmation = true)

    val truthPiecePersonCrush1True = Character(Name("A"), Some(Customer(None, None, None, Some(None), None)))
    val truthPiecePersonCrush1False = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("C"))), None)))
    val truthPiecePersonCrush2True = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("B"))), None)))
    val truthPiecePersonCrush2False = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("C"))), None)))

    sentencePersonCrush1.compareWithTruth(List(truthPiecePersonCrush1True)) should be (Some(true))
    sentencePersonCrush1.compareWithTruth(List(truthPiecePersonCrush1False)) should be (Some(false))
    sentencePersonCrush2.compareWithTruth(List(truthPiecePersonCrush2True)) should be (Some(true))
    sentencePersonCrush2.compareWithTruth(List(truthPiecePersonCrush2False)) should be (Some(false))
  }

  test("Checking sentences with customer partial truth song crush") {
    val sentenceSongCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(None))), directObjectAffirmation = true)
    val sentenceSongCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))), directObjectAffirmation = true)

    val truthPieceSongCrush1True = Character(Name("A"), Some(Customer(None, None, None, None, Some(None))))
    val truthPieceSongCrush1False = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Children of the Revolution", "T.Rex"))))))
    val truthPieceSongCrush2True = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPieceSongCrush2False = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Children of the Revolution", "T.Rex"))))))

    sentenceSongCrush1.compareWithTruth(List(truthPieceSongCrush1True)) should be (Some(true))
    sentenceSongCrush1.compareWithTruth(List(truthPieceSongCrush1False)) should be (Some(false))
    sentenceSongCrush2.compareWithTruth(List(truthPieceSongCrush2True)) should be (Some(true))
    sentenceSongCrush2.compareWithTruth(List(truthPieceSongCrush2False)) should be (Some(false))
  }

  test("Checking sentences with customer defined truth gender") {
    val sentenceGender = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(Some(Girl), None, None, None, None)), directObjectAffirmation = true)

    val truthPieceGenderTrue = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPieceGenderFalse = Character(Name("A"), Some(Customer(Some(Boy), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))

    sentenceGender.compareWithTruth(List(truthPieceGenderTrue)) should be (Some(true))
    sentenceGender.compareWithTruth(List(truthPieceGenderFalse)) should be (Some(false))
  }

  test("Checking sentences with customer defined truth sexual preference") {
    val sentenceSexualPreference = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, Some(Gay), None, None, None)), directObjectAffirmation = true)

    val truthSexualPreferenceTrue = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthSexualPreferenceFalse = Character(Name("A"), Some(Customer(Some(Girl), Some(Heterosexual), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))

    sentenceSexualPreference.compareWithTruth(List(truthSexualPreferenceTrue)) should be (Some(true))
    sentenceSexualPreference.compareWithTruth(List(truthSexualPreferenceFalse)) should be (Some(false))
  }

  test("Checking sentences with customer defined truth mood") {
    val sentenceMood = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, Some(Drunk), None, None)), directObjectAffirmation = true)

    val truthPieceMoodTrue = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPieceMoodFalse = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Jerk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))

    sentenceMood.compareWithTruth(List(truthPieceMoodTrue)) should be (Some(true))
    sentenceMood.compareWithTruth(List(truthPieceMoodFalse)) should be (Some(false))
  }

  test("Checking sentences with customer defined truth person crush") {
    val sentencePersonCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(None), None)), directObjectAffirmation = true)
    val sentencePersonCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, Some(Some(Name("B"))), None)), directObjectAffirmation = true)

    val truthPiecePersonCrush1True = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(None), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPiecePersonCrush1False = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("C"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPiecePersonCrush2True = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPiecePersonCrush2False = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("C"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))

    sentencePersonCrush1.compareWithTruth(List(truthPiecePersonCrush1True)) should be (Some(true))
    sentencePersonCrush1.compareWithTruth(List(truthPiecePersonCrush1False)) should be (Some(false))
    sentencePersonCrush2.compareWithTruth(List(truthPiecePersonCrush2True)) should be (Some(true))
    sentencePersonCrush2.compareWithTruth(List(truthPiecePersonCrush2False)) should be (Some(false))
  }

  test("Checking sentences with customer defined truth song crush") {
    val sentenceSongCrush1 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(None))), directObjectAffirmation = true)
    val sentenceSongCrush2 = Sentence(Name("A"), Name("A"), None, StateDirectObject(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))), directObjectAffirmation = true)

    val truthPieceSongCrush1True = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(None))))
    val truthPieceSongCrush1False = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Children of the Revolution", "T.Rex"))))))
    val truthPieceSongCrush2True = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPieceSongCrush2False = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), Some(Drunk), Some(Some(Name("B"))), Some(Some(Song("Children of the Revolution", "T.Rex"))))))

    sentenceSongCrush1.compareWithTruth(List(truthPieceSongCrush1True)) should be (Some(true))
    sentenceSongCrush1.compareWithTruth(List(truthPieceSongCrush1False)) should be (Some(false))
    sentenceSongCrush2.compareWithTruth(List(truthPieceSongCrush2True)) should be (Some(true))
    sentenceSongCrush2.compareWithTruth(List(truthPieceSongCrush2False)) should be (Some(false))
  }

  test("Checking sentences with worker") {
    val sentenceWorker = Sentence(Name("A"), Name("A"), None, StateDirectObject(Worker(Some(Waiter))), directObjectAffirmation = true)

    val truthPieceCustomer = Character(Name("A"), Some(Customer(None, None, None, None, None)))
    val truthPieceWorker = Character(Name("A"), Some(Worker(None)))
    val truthPieceWaiter = Character(Name("A"), Some(Worker(Some(Waiter))))
    val truthPieceDJ = Character(Name("A"), Some(Worker(Some(DJ))))

    sentenceWorker.compareWithTruth(List(truthPieceCustomer)) should be (Some(false))
    sentenceWorker.compareWithTruth(List(truthPieceWorker)) should be (Some(true))
    sentenceWorker.compareWithTruth(List(truthPieceWaiter)) should be (Some(true))
    sentenceWorker.compareWithTruth(List(truthPieceDJ)) should be (Some(false))
  }
}

class GayClubNonCopulativeVerbsTests extends GayClubTests {
  test("Like song positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Sweet Dreams", "Eurythmics")), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Children of the Revolution", "T.Rex"))))))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, None, None, None, Some(None))))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, None, None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (None)
  }

  test("Like song negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Sweet Dreams", "Eurythmics")), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Sweet Dreams", "Eurythmics"))))))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, None, None, None, Some(Some(Song("Children of the Revolution", "T.Rex"))))))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, None, None, None, Some(None))))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, None, None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (None)
  }

  test("Like person positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("B"))), None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("C"))), None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, None, None, Some(None), None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, None, None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (None)
  }

  test("Like person negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Name("B")), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("B"))), None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, None, None, Some(Some(Name("C"))), None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, None, None, Some(None), None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, None, None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (None)
  }

  test("Boys Like boys positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Boy), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Boy), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Boy), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Boy), Some(Asexual), None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Girls Like boys positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Girl), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Girl), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Girl), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Boys Like girls positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Boy), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Boy), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Boy), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Boy), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Girls Like girls positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Girl), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Girl), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Girl), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Undefined Like boys positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Undefined Like girls positive tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = true)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(false))
  }

  test("Boys Like boys negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Boy), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Boy), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Boy), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Boy), Some(Asexual), None, None, None)))

    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }

  test("Girls Like boys negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Girl), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Girl), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Girl), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }

  test("Boys Like girls negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Boy), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Boy), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Boy), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Boy), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }

  test("Girls Like girls negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(Some(Girl), Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(Some(Girl), Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(Some(Girl), Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(Some(Girl), Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (Some(true))
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }

  test("Undefined Like boys negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Boy), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }

  test("Undefined Like girls negative tests") {
    val sentence = Sentence(Name("A"), Name("A"), Some(Like), ReferenceDirectObject(Girl), directObjectAffirmation = false)
    val truthPiece1 = Character(Name("A"), Some(Customer(None, Some(Gay), None, None, None)))
    val truthPiece2 = Character(Name("A"), Some(Customer(None, Some(Heterosexual), None, None, None)))
    val truthPiece3 = Character(Name("A"), Some(Customer(None, Some(Bisexual), None, None, None)))
    val truthPiece4 = Character(Name("A"), Some(Customer(None, Some(Asexual), None, None, None)))


    checkNonCopulative(sentence, List(truthPiece1), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece2), 0) should be (None)
    checkNonCopulative(sentence, List(truthPiece3), 0) should be (Some(false))
    checkNonCopulative(sentence, List(truthPiece4), 0) should be (Some(true))
  }
}

class GayClubStateTests extends GayClubTests {
  test("Define possible states from conversation"){
    val conversation1 = List(
      Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true),
      Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("T.Rex", "Children of the revolution")), directObjectAffirmation = true)
    )
    val conversation2 = List(
      Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true),
      Sentence(Name("A"), Name("A"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    )
    val conversation3 = List()
    val songs1 = List(UnknownSong, Song("Eurythmics", "Sweet Dreams"), Song("T.Rex", "Children of the revolution"))
    val songs2 = List(UnknownSong, Song("Eurythmics", "Sweet Dreams"))
    val songs3 = List(UnknownSong)

    GayClub.possibleWorldStates(None, conversation1) should be (songs1)
    GayClub.possibleWorldStates(None, conversation2) should be (songs2)
    GayClub.possibleWorldStates(None, conversation3) should be (songs3)
  }

  test("When no input, races should be returned"){
    val limitedRaces = GayClub.races()

    limitedRaces.collect{case Customer(Some(Girl), _, _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(Some(Boy), _, _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, Some(Gay), _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, Some(Heterosexual), _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, Some(Bisexual), _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, Some(Gay), _, _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, _, Some(Sober), _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, _, Some(Drunk), _, _) => 1}.sum > 0 should be (true)
    limitedRaces.collect{case Customer(_, _, Some(Jerk), _, _) => 1}.sum > 0 should be (true)
    limitedRaces.contains(Worker(Some(Waiter))) should be (true)
    limitedRaces.contains(Worker(Some(DJ))) should be (true)
  }

  test("Define possible races from conversations"){
    val conversation = List(
      Sentence(Name("A"), Name("B"), Some(Like), ReferenceDirectObject(Name("C")), directObjectAffirmation = true),
      Sentence(Name("A"), Name("B"), Some(Like), StateDirectObject(Song("Eurythmics", "Sweet Dreams")), directObjectAffirmation = true)
    )

    val races = GayClub.races(conversation)

    races.collect{case Customer(Some(Girl), _, _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(Some(Boy), _, _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, Some(Gay), _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, Some(Heterosexual), _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, Some(Bisexual), _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, Some(Gay), _, _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, Some(Sober), _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, Some(Drunk), _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, Some(Jerk), _, _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, Some(Some(Name("A"))), _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, Some(Some(Name("B"))), _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, Some(Some(Name("C"))), _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, Some(None), _) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, _, Some(Some(Song("Eurythmics", "Sweet Dreams")))) => 1}.sum > 0 should be (true)
    races.collect{case Customer(_, _, _, _, Some(None)) => 1}.sum > 0 should be (true)
    races.contains(Worker(Some(Waiter))) should be (true)
    races.contains(Worker(Some(DJ))) should be (true)
  }

  test("World states should return just the current song"){
    GayClub.possibleWorldAspects(None) should be (List(SongReference(0)))
  }

  test("World states depend on the conversation"){
    val conversation1 = List(
      Sentence(Name("DJ"), Name("A"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true),
      Sentence(Name("DJ"), Name("A"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true),
      Sentence(Name("DJ"), Name("A"), Some(Like), ReferenceDirectObject(SongReference(3)), directObjectAffirmation = true))
    val conversation2 = List(
      Sentence(Name("DJ"), Name("A"), Some(Like), ReferenceDirectObject(SongReference(3)), directObjectAffirmation = false),
      Sentence(Name("DJ"), Name("A"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = false))

    GayClub.possibleWorldAspects(None, conversation1) should be (List(SongReference(0), SongReference(1), SongReference(2), SongReference(3)))
    GayClub.possibleWorldAspects(None, conversation2) should be (List(SongReference(0), SongReference(1), SongReference(2), SongReference(3)))
  }

  test("There cannot be different songs in the playlist"){
    val truth1 = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution")))
    )
    val truth2 = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("Eurythmics", "Sweet Dreams")))
    )
    val truth3 = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams")))
    )

    GayClub.checkConsistency(truth1) should be (true)
    GayClub.checkConsistency(truth2) should be (false)
    GayClub.checkConsistency(truth3) should be (true)
  }

}

class GayClubPossibleSentencesTests extends GayClubTests {
  test("Anyone can speak about this song"){
    val truth = List(
      Character(Name("Customer"), Some(Customer(None, None, None, None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None))),
      Character(Name("Undefined"), None)
    )
    val customerSentence = Sentence(Name("Customer"), Name("Customer"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)
    val djSentence = Sentence(Name("DJ"), Name("Customer"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)
    val waiterSentence = Sentence(Name("Waiter"), Name("Customer"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)
    val workerSentence = Sentence(Name("Worker"), Name("Customer"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)
    val undefinedSentence = Sentence(Name("Undefined"), Name("Customer"), Some(Like), ReferenceDirectObject(SongReference(0)), directObjectAffirmation = true)

    Truth.sentenceCanBeSpoken(GayClub, truth, List(customerSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(djSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(waiterSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(workerSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(undefinedSentence), 0) should be (true)
  }

  test("Only DJs can speak about songs in the future"){
    val truth = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution"))),
      Character(Name("CustomerSober"), Some(Customer(None, None, Some(Sober), None, None))),
      Character(Name("CustomerJerk"), Some(Customer(None, None, Some(Jerk), None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None))),
      Character(Name("Undefined"), None)
    )
    val customerSoberSentence = Sentence(Name("CustomerSober"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val customerJerkSentence = Sentence(Name("CustomerJerk"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val djSentence = Sentence(Name("DJ"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val waiterSentence = Sentence(Name("Waiter"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val workerSentence = Sentence(Name("Worker"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)
    val undefinedSentence = Sentence(Name("Undefined"), Name("CustomerSober"), Some(Like), ReferenceDirectObject(SongReference(1)), directObjectAffirmation = true)

    Truth.sentenceCanBeSpoken(GayClub, truth, List(customerSoberSentence), 0) should be (false)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(customerJerkSentence), 0) should be (false)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(djSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(waiterSentence), 0) should be (false)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(workerSentence), 0) should be (true)
    Truth.sentenceCanBeSpoken(GayClub, truth, List(undefinedSentence), 0) should be (true)
  }

  test("No one can say someone likes her/himself"){
    val truth = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution"))),
      Character(Name("CustomerSober"), Some(Customer(None, None, Some(Sober), None, None))),
      Character(Name("CustomerJerk"), Some(Customer(None, None, Some(Jerk), None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None)))
    )

    val names =
      List(Name("CustomerSober"), Name("CustomerJerk"), Name("DJ"), Name("Waiter"), Name("Worker"))

    val sentences =
      for{
        name1 <- names
        name2 <- names
        directObjectAffirmation <- List(true, false)
      } yield Sentence(name1, name2, Some(Like), ReferenceDirectObject(name2), directObjectAffirmation)

    sentences.indices.exists(Truth.sentenceCanBeSpoken(GayClub, truth, sentences, _)) should be (false)
  }

  test("Nobody cannot talk about who a worker likes"){
    val truth = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution"))),
      Character(Name("CustomerSober"), Some(Customer(None, None, Some(Sober), None, None))),
      Character(Name("CustomerJerk"), Some(Customer(None, None, Some(Jerk), None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None)))
    )

    val names =
      List(Name("CustomerSober"), Name("CustomerJerk"), Name("DJ"), Name("Waiter"), Name("Worker"))

    val sentences =
      for{
        name <- names
        workerName <- List(Name("DJ"), Name("Waiter"), Name("Worker"))
        directObjectAffirmation <- List(true, false)
      } yield Sentence(name, workerName, Some(Like), ReferenceDirectObject(Name("CustomerSober")), directObjectAffirmation)

    sentences.indices.exists(Truth.sentenceCanBeSpoken(GayClub, truth, sentences, _)) should be (false)
  }

  test("Workers can speak about customers"){
    val truth = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution"))),
      Character(Name("CustomerSober"), Some(Customer(None, None, Some(Sober), None, None))),
      Character(Name("CustomerJerk"), Some(Customer(None, None, Some(Jerk), None, None))),
      Character(Name("CustomerDrunk"), Some(Customer(None, None, Some(Drunk), None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None)))
    )

    val sentences =
      for{
        customerName <- List(Name("CustomerSober"), Name("CustomerJerk"), Name("CustomerDrunk"))
        workerName <- List(Name("DJ"), Name("Waiter"), Name("Worker"))
        likePossibility <- List(ReferenceDirectObject(Name("Waiter")),StateDirectObject(Song("Eurythmics", "Sweet Dreams")))
        directObjectAffirmation <- List(true, false)
      } yield Sentence(workerName, customerName, Some(Like), likePossibility, directObjectAffirmation)

    sentences.indices.forall(Truth.sentenceCanBeSpoken(GayClub, truth, sentences, _)) should be (true)
  }

  test("Workers like all the songs"){
    val truth = List(
      WorldAspect(SongReference(0), Some(Song("Eurythmics", "Sweet Dreams"))),
      WorldAspect(SongReference(1), Some(Song("T.Rex", "Children of the revolution"))),
      Character(Name("CustomerSober"), Some(Customer(None, None, Some(Sober), None, None))),
      Character(Name("CustomerJerk"), Some(Customer(None, None, Some(Jerk), None, None))),
      Character(Name("CustomerDrunk"), Some(Customer(None, None, Some(Drunk), None, None))),
      Character(Name("DJ"), Some(Worker(Some(DJ)))),
      Character(Name("Waiter"), Some(Worker(Some(Waiter)))),
      Character(Name("Worker"), Some(Worker(None)))
    )

    val sentences =
      for{
        workerName <- List(Name("DJ"), Name("Waiter"), Name("Worker"))
        likePossibility <- List(StateDirectObject(Song("Eurythmics", "Sweet Dreams")), StateDirectObject(Song("T.Rex", "Children of the revolution")))
      } yield (Sentence(workerName, workerName, Some(Like), likePossibility, directObjectAffirmation = true), Sentence(workerName, workerName, Some(Like), likePossibility, directObjectAffirmation = false))

    val (positiveSentences, negativeSentences) = (sentences.map(_._1), sentences.map(_._2))

    positiveSentences.indices.forall(Truth.sentenceCanBeSpoken(GayClub, truth, positiveSentences, _)) should be (true)
    negativeSentences.indices.exists(Truth.sentenceCanBeSpoken(GayClub, truth, negativeSentences, _)) should be (false)
  }

}