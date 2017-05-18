package TruthEngine

import TruthEngine.Language._

object Parser {

  val invalidCharacterNames:List[String] = List("someone", "it", "i")

  def parseText[W <:World[W]](world:W, raw_text:List[String]):Either[String, (Truth[W], List[Sentence])] = {
    val eitherListSentences:Either[String, List[Sentence]] =
      raw_text.map(raw_sentence => parseSentence(world, raw_sentence))
        .foldLeft[Either[String, List[Sentence]]](Right(List())){
        case (eitherList, eitherSentence) =>
          for{
            list <- eitherList
            sentence <- eitherSentence
          } yield sentence::list
      }.map(_.reverse)

    eitherListSentences.flatMap{
      listSentences =>
        val listCharacters:List[Name] = (
          listSentences.map(_.speaker) ++
            listSentences.map(_.subject).collect{case name:Name => name})
          .distinct.sortBy(_.charName)
        listCharacters.find(ch => invalidCharacterNames.contains(ch.charName.trim.toLowerCase)) match {
          case Some(invalidCharacter) =>
            Left(s"The text contains the invalid character name '${invalidCharacter.charName}'")
          case None =>
            val truthPieces = world.possibleWorldAspects().map(WorldAspect(_, None)) ++ listCharacters.map(Character(_, None))
            Right((Truth(world, truthPieces), listSentences))
        }
    }
  }

  def printResult[W <:World[W]](listTruth:List[Truth[W]], oneSolution:Boolean = false):String = {
    if(oneSolution)
      listTruth.headOption.fold("There are no possible truths")( head => {
        val summaryTruth:Truth[W] = listTruth.tail.foldLeft(head)(Truth.merge)
        s"There are ${listTruth.length} possible truths. " ++
          {
            if(summaryTruth.truthPieces.exists(tp => tp.state.isDefined))
              "And summarizing, this is what we know for sure:\n" ++
                parseTruth(summaryTruth)
            else
              "But as Socrates we claim just to know that we know nothing."
          }
      })
    else
    if(listTruth.isEmpty)
      "There are no possible truths"
    else
      s"There are ${listTruth.length} possible truths:\n" ++
        listTruth.zipWithIndex.map{
          case (truth, index) =>
            s"TruthEngine.Truth number ${index + 1}\n" ++
              parseTruth(truth)
        }.mkString("\n\n")
  }

  private def parseSentence[W <:World[W]](world:W, raw_sentence:String):Either[String, Sentence] = {
    val sentenceRegex = """(\w+): (I am|Someone is|Everyone is|No one is|There are at least \d+|There are exactly \d+|It is|\w+ is)( not | )(\w+)""".r
    raw_sentence match {
      case sentenceRegex(speaker, raw_subject_verb, maybeNot, raw_directObject) =>
        for{
          directObject <- parseDirectObject(raw_directObject, world.races ++ world.possibleWorldStates())
          subject <- parseSubject(world, speaker, raw_subject_verb, directObject)
          sentenceAffirmation <- parseSentenceAffirmation(raw_subject_verb)
          directObjectAffirmation <- parseDirectObjectAffirmation(maybeNot)
        } yield Sentence(Name(speaker), subject, directObject, sentenceAffirmation, directObjectAffirmation)
      case _ =>
        Left(s"Sentence '$raw_sentence' has not the correct grammar")
    }
  }

  private def parseSubject[W <:World[W]](world:W, speaker:String, raw_subject_verb:String, directObject:State):Either[String, Reference[State]] = {
    val atLeastRegex = """(There are at least) (\d+)""".r
    val exactlyRegex = """(There are exactly) (\d+)""".r
    val nameRegex = """(\w+) (is)""".r

    raw_subject_verb match {
      case "I am" =>
        Right(Name(speaker))
      case "Someone is" =>
        Right(NumberOfPeople(1, isExact = false))
      case "Everyone is" =>
        Right(Everyone)
      case "No one is" =>
        Right(NumberOfPeople(1, isExact = false))
      case "It is" =>
        directObject match {
          case ws:WorldState[W]@unchecked =>
            world.possibleWorldAspects(Some(ws)).headOption
              .fold[Either[String, Reference[State]]](Left(s"TruthEngine.World is badly defined"))(Right.apply)
          case _ =>
            Left(s"Sentence s'$directObject' is a wrong reference to a wrong world state")
        }
      case atLeastRegex(_, n) =>
        Right(NumberOfPeople(n.toInt, isExact = false))
      case exactlyRegex(_, n) =>
        Right(NumberOfPeople(n.toInt, isExact = true))
      case nameRegex(name, _) =>
        Right(Name(name))
    }
  }

  private def parseDirectObject(raw_directObject:String, possibleDirectObjects:List[State]):Either[String, State] = {
    val statesMap = /*(races ++ possibleWorldStates())*/possibleDirectObjects.map(r => r.stringRef -> r).toMap
    statesMap.get(raw_directObject).fold[Either[String, State]](Left(s"Direct object $raw_directObject is invalid"))(Right.apply)
  }

  private def parseDirectObjectAffirmation(maybeNot:String):Either[String, Boolean] =
    Right(maybeNot != " not ")

  private def parseSentenceAffirmation(raw_subject_verb:String):Either[String, Boolean] =
    raw_subject_verb match {
      case "No one is" =>
        Right(false)
      case _ =>
        Right(true)
    }

  private def parseTruth[W <:World[W]](truth:Truth[W]):String =
    truth.truthPieces.map(tp => (tp.reference, tp.state)).collect{
      case (wsr:WorldAspectReference[_, _], Some(ws)) =>
        s"It is $ws"
      case (Name(charName), Some(race)) =>
        s"$charName is $race"
    }.mkString("\n")
}
