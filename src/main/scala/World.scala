import Language._

trait World[W <: World[W]] {
  val worldInstance:W
  val description:String
  def findTruth(raw_text:List[String], oneSolution:Boolean):String =
    parseText(raw_text).map {
      case (socratesTruth, text) =>
        Truth.compareTextAndTruth(socratesTruth, text, oneSolution)
    } match {
      case Right(listTruth) =>
        parseResult(listTruth)
      case Left(error) =>
        error
    }

  def getPossibles(s:Reference[_]):List[TruthPiece[State]] = s match {
    case war:WorldAspectReference[W, WorldState[W]]@unchecked =>
      possibleWorldStates(Some(war)).map(ws => WorldAspect(war, Some(ws)))
    case name:Name =>
      races.map(r => Character(name, Some(r)))
    case _ =>
      List()
  }

  protected def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None):List[WorldState[W]]
  protected def possibleWorldAspects(ws:Option[WorldState[W]] = None):List[WorldAspectReference[W, WorldState[W]]]
  protected val races:List[Race]

  private def parseText(raw_text:List[String]):Either[String, (Truth[W], List[Sentence[W]])] = {
    val eitherListSentences:Either[String, List[Sentence[W]]] =
      raw_text.map(raw_sentence => parseSentence(raw_sentence))
        .foldLeft[Either[String, List[Sentence[W]]]](Right(List())){
          case (eitherList, eitherSentence) =>
            for{
              list <- eitherList
              sentence <- eitherSentence
            } yield sentence::list
        }.map(_.reverse)

    //Character name cannot be "It", "Someone"
    //Race cannot be "not"
    //After trimming and lowercase it, worldstates, races and characternames cannot have duplicates
    eitherListSentences.map{
      listSentences =>
        val listCharacters:List[Name] = (
          listSentences.map(_.speaker) ++
            listSentences.map(_.directObject).collect{case name:Name => name})
          .distinct.sortBy(_.charName)
        val truthPieces = possibleWorldAspects().map(WorldAspect(_, None)) ++ listCharacters.map(Character(_, None))
        (Truth(worldInstance, truthPieces), listSentences)
    }


  }

  private def parseSentence(raw_sentence:String):Either[String, Sentence[W]] = {
    def parseSubject(speaker:String, raw_subject:String, directObject:State):Either[String, Reference[State]] = {
      val atLeastRegex = """(There are at least) (\d+)""".r
      val exactlyRegex = """(There are exactly) (\d+)""".r
      val nameRegex = """(\w+) (is)""".r

      raw_subject match {
        case "I am" =>
          Right(Name(speaker))
        case "Someone is" =>
          Right(NumberOfPeople(1, isExact = false))
        case "No one is" =>
          Right(Everyone)
        case "It is" =>
          directObject match {
            case ws:WorldState[W]@unchecked =>
              possibleWorldAspects(Some(ws)).headOption
                .fold[Either[String, Reference[State]]](Left(s"World is badly defined"))(Right.apply)
            case _ =>
              Left(s"Sentence s'$raw_sentence' references a wrong world state")
          }
        case atLeastRegex(_, n) =>
          Right(NumberOfPeople(n.toInt, isExact = false))
        case exactlyRegex(_, n) =>
          Right(NumberOfPeople(n.toInt, isExact = true))
        case nameRegex(name, _) =>
          Right(Name(name))
      }
    }

    def parseDirectObject(raw_directObject:String):Either[String, State] = {
      val statesMap = (races ++ possibleWorldStates()).map(r => r.stringRef -> r).toMap
      statesMap.get(raw_directObject).fold[Either[String, State]](Left(s"Direct object $raw_directObject is invalid"))(Right.apply)
    }

    val sentenceRegex = """(\w+): (I am|Someone is|No one is|There are at least \d+|There are exactly \d+|It is|\w+ is)( not | )(\w+)""".r
    raw_sentence match {
      case sentenceRegex(speaker, raw_subject, maybeNot, raw_directObject) =>
        for{
          directObject <- parseDirectObject(raw_directObject)
          subject <- parseSubject(speaker, raw_subject, directObject)
        } yield Sentence(Name(speaker), subject, directObject, affirmative = maybeNot != " not ")

      case _ =>
        Left(s"Sentence '$raw_sentence' has not the correct grammar")
    }
  }

  private def parseResult(listTruth:List[Truth[W]]):String = {
    s"There are ${listTruth.length} possible truths:\n" ++
    listTruth.zipWithIndex.map{
      case (truth, index) =>
        s"Truth number ${index + 1}\n" ++
        truth.truthPieces.map(tp => (tp.reference, tp.state)).collect{
          case (wsr:WorldAspectReference[_, _], Some(ws)) =>
            s"It is $ws"
          case (Name(charName), Some(race)) =>
            s"$charName is $race"
        }.mkString("\n")
    }.mkString("\n\n")
  }


}
