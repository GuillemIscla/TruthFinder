package TruthEngine

import TruthEngine.Language._

trait Parser[W <:World[W]] extends Translator[String, Sentence, (Truth[W], List[Sentence])]{
  val world:W
  val forbiddenNames:List[String]
  val parserName:String

  def translateScriptSentence(raw_script_sentence: String): Translation[String, Sentence]

  def script_general_check(resultList:List[Sentence]): Translation[List[Sentence], (Truth[W], List[Sentence])] = {
    val listCharacters: List[Name] =
        (resultList.map(_.speaker) ++ resultList.map(_.subject))
          .collect { case name: Name => name }.distinct.sortBy(_.charName)
      listCharacters.find(ch => forbiddenNames.contains(ch.charName.trim.toLowerCase)) match {
        case Some(invalidCharacter) =>
          translationError(s"""Sentence where the character named: "${invalidCharacter.charName}" speaks or is mentioned""", "Is an invalid name")
        case None =>
          val truthPieces = world.possibleWorldAspects().map(WorldAspect(_, None)) ++ listCharacters.map(Character(_, None))
          Translated((Truth(world, truthPieces), resultList))
      }
    }

  protected def translationError[Script, Result](bad_script:String, parserProblemWithScript:String):TranslationError[Script, Result] =
    TranslationError(bad_script, s"$parserName claimed that: $parserProblemWithScript")
}

case class ParserCollection[W <:World[W]](world:W, translatorList:List[Parser[W]]) extends Parser[W] with TranslatorCollection[String, Sentence, (Truth[W], List[Sentence])]{
  val parserName:String = translatorList.map(_.parserName).mkString("ParserCollection which contains Parsers(", ", ", ")")
  val forbiddenNames:List[String] = translatorList.foldLeft(List.empty[String])({
    case (fn, p) =>
      fn ++ p.forbiddenNames
  })
}

object ParserHelper {//With the parsers here the non-copulative verbs are parsed
  case class GenericParser[W <:World[W]](world:W) extends Parser[W] {
    val parserName:String = "GenericParser"
    val forbiddenNames:List[String] = List("i", "someone", "everyone", "no one", "there", "are", "exactly", "am", "not")

    def translateScriptSentence(raw_script_sentence:String):Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (I am|Someone is|Everyone is|No one is|There are at least \d+|There are exactly \d+|\w+ is)( not | )(\w+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, raw_subject_verb, maybeNot, raw_directObject) =>
          for{
            subject <- parseSubject(speaker, raw_subject_verb)
            directObject <- parseDirectObject(raw_script_sentence, raw_directObject, world.races)
            directObjectAffirmation <- parseDirectObjectAffirmation(maybeNot)
          } yield Sentence(Name(speaker), subject, directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseSubject(speaker:String, raw_subject_verb:String):Translation[String, Reference[State]] = {
      val atLeastRegex = """(There are at least) (\d+)""".r
      val exactlyRegex = """(There are exactly) (\d+)""".r
      val nameRegex = """(\w+) (is)""".r

      raw_subject_verb match {
        case "I am" =>
          Translated(Name(speaker))
        case "Someone is" =>
          Translated(NumberOfPeople(1, isExact = false))
        case "Everyone is" =>
          Translated(Everyone)
        case "No one is" =>
          Translated(NumberOfPeople(0, isExact = true))
        case atLeastRegex(_, n) if n.toInt < 0 =>
          translationError(raw_subject_verb, "this 'atLeast' expression contains a negative number")
        case atLeastRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, isExact = false))
        case exactlyRegex(_, n) if n.toInt < 0 =>
          translationError(raw_subject_verb, "this 'exactly' expression contains a negative number")
        case exactlyRegex(_, n) =>
          Translated(NumberOfPeople(n.toInt, isExact = true))
        case nameRegex(name, _) =>
          Translated(Name(name))
        case _ =>
          translationError(raw_subject_verb, "thought it could translate this subject + verb but failed")
      }
    }

    private def parseDirectObject(raw_sentence:String, raw_directObject:String, possibleRaces:List[Race]):Translation[String, Race] =
      possibleRaces.map(r => r.stringRef -> r).toMap.get(raw_directObject).fold[Translation[String, Race]](
        translationError(raw_directObject, "thought it could translate this direct object but failed")
      )(Translated.apply)

    private def parseDirectObjectAffirmation(maybeNot:String):Translation[String, Boolean] =
      Translated(maybeNot != " not ")
  }

  case class RegularWorldState[W <:World[W]](world:W) extends Parser[W] {
    val parserName:String = "RegularWorldState"
    val forbiddenNames: List[String] = List("it")

    def translateScriptSentence(raw_script_sentence: String): Translation[String, Sentence] = {
      val sentenceRegex = """(\w+): (It is)( not|) (\w+)""".r
      raw_script_sentence match {
        case sentenceRegex(speaker, _, maybeNot, raw_directObject) =>
          for {
            subjectDirectObject <- parseSubjectDirectObject(raw_directObject, world.possibleWorldStates(None))
            (subject, directObject) = subjectDirectObject
            directObjectAffirmation <- parseDirectObjectAffirmation(maybeNot)
          } yield Sentence(Name(speaker), subject, directObject, directObjectAffirmation)
        case _ =>
          NotTranslated(raw_script_sentence)
      }
    }

    private def parseSubjectDirectObject(raw_directObject:String, possibleWorldStates:List[WorldState[W]]): Translation[String, (WorldAspectReference[W, WorldState[W]], WorldState[W])] = {
      val statesMap = possibleWorldStates.map(r => r.stringRef -> r).toMap
      statesMap.get(raw_directObject).fold[Translation[String, (WorldAspectReference[W, WorldState[W]], WorldState[W])]](TranslationError(raw_directObject, "Parser RegularWorldState claims that this direct object is an invalid direct object")){
        case wst: WorldState[W]@unchecked =>
          world.possibleWorldAspects(Some(wst)).headOption
            .fold[Translation[String, (WorldAspectReference[W, WorldState[W]], WorldState[W])]](
              translationError(s"Creator of the world: $world speech", s"the possibleWorldAspects function of the world: $world is badly defined")
            )(was => Translated((was, wst)))
        case _ =>
          translationError(raw_directObject, "this direct object references to a wrong world state")
      }
    }

    private def parseDirectObjectAffirmation(maybeNot:String):Translation[String, Boolean] =
      Translated(maybeNot != " not ")
  }

}
