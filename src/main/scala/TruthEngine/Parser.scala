package TruthEngine

import TruthEngine.Language._
import TruthEngine.Truth.Truth

import scala.reflect.ClassTag

trait ParserBase[W <:World[W]] {
  val world:W
  val forbiddenNames:List[String]
  val parserName:String
}

trait LineParser[W <:World[W]] extends Translator[String, Sentence] with ParserBase[W]

case class TextParser[W <:World[W]](world:W, translatorCollection:List[LineParser[W]]) extends TextTranslator[String, Sentence,(Truth, List[Sentence])] with ParserBase[W] {
  val parserName:String = s"TextParser containing ${translatorCollection.map(_.parserName).mkString("(",",",")")}"
  val forbiddenNames:List[String] =
    translatorCollection.foldLeft[List[String]](List()){
      case (acc, otherTranslator) =>
        acc ++ otherTranslator.forbiddenNames
    }
  val scriptTag:ClassTag[String] = implicitly[ClassTag[String]]

  def generalCheck(listTranslated:List[Sentence]):Translation[List[Sentence], List[Sentence]] = {
    listCharacters(listTranslated).find(ch => forbiddenNames.contains(ch.charName.trim.toLowerCase)) match {
      case Some(invalidCharacter) =>
        TranslationError.invalidCharacterError(invalidCharacter.charName)
      case None =>
        Translated(listTranslated)
    }
  }

  def formatResult(listTranslated:List[Sentence]):(Truth, List[Sentence]) = {
    val socratesTruth:Truth =
      world.possibleWorldAspects().map(WorldAspect(_, None)) ++
        listCharacters(listTranslated).map(Character(_, None))
    (socratesTruth, listTranslated)
  }

  private def listCharacters(listTranslated:List[Sentence]): List[Name] =
    (listTranslated.map(_.speaker) ++ listTranslated.map(_.subject) ++ listTranslated.map(_.directObject))
      .collect { case name: Name => name }.distinct.sortBy(_.charName)

}

object ParserHelper {//With the parsers here the non-copulative verbs are parsed
  case class GenericParser[W <:World[W]](world:W) extends LineParser[W] {
    val parserName:String = "GenericParser"
    val forbiddenNames:List[String] = List("i", "someone", "everyone", "no one", "there", "are", "exactly", "am", "not")

    def translate(script:String):Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (I am|Someone is|Everyone is|No one is|There (is|are) (at least|at most|exactly) \d+|\w+ is)( not|) (.+)""".r
      script match {
        case sentenceRegex(speaker, raw_subject_verb, _, _, maybeNot, raw_directObject) =>
          (for{
            _ <- NotTranslated[String, Sentence](script)
            subject <- parseSubject(speaker, raw_subject_verb).newTranslation[Sentence]
            directObject <- parseDirectObject(script, raw_directObject, world.races).newTranslation[Sentence]
            directObjectAffirmation <- parseDirectObjectAffirmation(maybeNot)
          } yield Sentence(Name(speaker), subject, directObject, directObjectAffirmation)).translateFrom[String]
        case _ =>
          NotTranslated(script)
      }
    }

    private def parseSubject(speaker:String, raw_subject_verb:String):Translation[String, Reference[State]] = {
      val atLeastRegex = """(There are at least|There is at least) (\d+)""".r
      val atMostRegex = """(There are at most|There is at most) (\d+)""".r
      val exactlyRegex = """(There are exactly|There is exactly) (\d+)""".r
      val nameRegex = """(\w+) (is)""".r

      raw_subject_verb match {
        case "I am" =>
          Translated(Name(speaker))
        case "Someone is" =>
          Translated(NumberOfPeople(1, MoreOrEqual))
        case "Everyone is" =>
          Translated(Everyone)
        case "No one is" =>
          Translated(NumberOfPeople(0, Exactly))
        case atLeastRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, MoreOrEqual))
        case atMostRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, LessOrEqual))
        case exactlyRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, Exactly))
        case nameRegex(name, _) =>
          Translated(Name(name))
        case _ =>
          TranslationError(raw_subject_verb, "thought it could translate this subject + verb but failed")
      }
    }

    private def parseDirectObject(raw_sentence:String, raw_directObject:String, possibleRaces:List[Race]):Translation[String, Race] =
      possibleRaces.map(r => r.stringRef -> r).toMap.get(raw_directObject).fold[Translation[String, Race]](
        TranslationError(raw_directObject, "thought it could translate this direct object but failed")
      )(Translated.apply)

    private def parseDirectObjectAffirmation(maybeNot:String):Translation[String, Boolean] =
      Translated(maybeNot != " not")
  }

  case class RegularWorldStateParser[W <:World[W]](world:W) extends LineParser[W] {
    val parserName:String = "RegularWorldState"
    val forbiddenNames: List[String] = List("it")

    def translate(script: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (It is)( not|) (.+)""".r
      script match {
        case sentenceRegex(speaker, _, maybeNot, raw_directObject) =>
          for {
            _ <- NotTranslated[String, Sentence](script)
            subject <- parseSubject(raw_directObject, world.possibleWorldStates(None)).newTranslation[Sentence]
            directObject <- parseDirectObject(raw_directObject, world.possibleWorldStates(None)).newTranslation[Sentence]
            directObjectAffirmation <- parseDirectObjectAffirmation(maybeNot)
          } yield Sentence(Name(speaker), subject, directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(script)
      }
    }

    private def parseSubject(raw_directObject:String, possibleWorldStates:List[WorldState[W]]): Translation[String, WorldAspectReference[W, WorldState[W]]] = {
      val statesMap = possibleWorldStates.map(r => r.stringRef -> r).toMap
      statesMap.get(raw_directObject).fold[Translation[String, WorldAspectReference[W, WorldState[W]]]](TranslationError(raw_directObject, s"$raw_directObject is an invalid direct object")){
        case wst: WorldState[W]@unchecked =>
          world.possibleWorldAspects(Some(wst)).headOption
            .fold[Translation[String, WorldAspectReference[W, WorldState[W]]]](
            TranslationError(s"Creator of the world: $world speech", s"the possibleWorldAspects function of the world: $world is badly defined")
            )(Translated.apply)
        case _ =>
          TranslationError(raw_directObject, "this direct object references to a wrong world state")
      }
    }

    private def parseDirectObject(raw_directObject:String, possibleWorldStates:List[WorldState[W]]): Translation[String, WorldState[W]] = {
      val statesMap = possibleWorldStates.map(r => r.stringRef -> r).toMap
      statesMap.get(raw_directObject).fold[Translation[String, WorldState[W]]](TranslationError(raw_directObject, s"$raw_directObject is an invalid direct object")){
        case wst: WorldState[W]@unchecked =>
          world.possibleWorldAspects(Some(wst)).headOption
            .fold[Translation[String, WorldState[W]]](
            TranslationError(s"Creator of the world: $world speech", s"the possibleWorldAspects function of the world: $world is badly defined")
          )(_ => Translated(wst))
        case _ =>
          TranslationError(raw_directObject, "this direct object references to a wrong world state")
      }
    }

    private def parseDirectObjectAffirmation(maybeNot:String):Translation[String, Boolean] =
      Translated(maybeNot != " not")
  }

}
