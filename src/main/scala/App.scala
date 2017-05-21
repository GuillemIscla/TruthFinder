import TruthEngine._
import Worlds._
import Language._
import scala.io.Source


object App {
  def main(args: Array[String]): Unit = {
    val world = GHE
    val conversationNumber = 8
    val fileName = s"src\\main\\resources\\${world.name}\\Conversation$conversationNumber.txt"
    val conversation =
      Source.fromFile(fileName).getLines().toList


    println("----------------")
    println(UserInterface.printWorld(world))
    println("----------------")
    println(fileName)
    println()
    println(conversation.mkString("\n"))
    println()
    println("****RESULT****")
    println(UserInterface.printTruth(world, world.findTruth(conversation), oneSolution = true))
    println("----------------")
    Thread.sleep(1000)
  }
}

object UserInterface{
  def printWorld[W <:World[W]](world:World[W]):String =
    s"Welcome to ${world.name} :D" + "\n" +
      world.description + "\n\n" +
      world.possibleWorldStates(None).map(ws => s"${ws.stringRef}: ${ws.description}").mkString("These are the world states:\n","\n","\n\n") +
      world.races.map(ws => s"${ws.stringRef}: ${ws.description}").mkString("These are the races in the world:\n","\n","\n\n")

  def printTruth[W <:World[W]](world:World[W], translatedListTruth:Translation[List[String], List[Truth[W]]], oneSolution:Boolean):String =
    translatedListTruth.flatMap(AppTranslator(world, oneSolution).translateFullScript) match {
      case Translated(result) =>
        result
      case NotTranslated(script) =>
        s"There has been an error since the script '$script' was not understood by the parsers"
      case TranslationError(bad_script, error) =>
        s"There has been an error since the script '$bad_script' caused the error '$error'"
    }

  case class AppTranslator[W <:World[W]](world:World[W], oneSolution:Boolean) extends Translator[Truth[W], Truth[W], String]{
    def translateScriptSentence(raw_script_sentence: Truth[W]): Translation[Truth[W], Truth[W]] = Translated(raw_script_sentence)
    def script_general_check(resultList:List[Truth[W]]): Translation[List[Truth[W]], String] =
      resultList.headOption.fold[Translation[List[Truth[W]], String]](Translated("There are no possible truths"))(
        head => {
          val transIntroduction = Translated[List[Truth[W]], String](s"There are ${resultList.length} possible truths.\n")
          if (oneSolution){
                for {
                  introduction <- transIntroduction
                  mergedTruth = resultList.tail.foldLeft(head){case (tp1, tp2) => Truth.merge(tp1, tp2, world.customMerge)}.truthPieces.filter(_.state.isDefined)
                  summary <- summarize(mergedTruth)
                } yield introduction ++ summary
          }
          else {
            for {
              introduction <- transIntroduction
              listTruthWithIndexes <- listTruthsWithIndexes(resultList)
            } yield introduction ++ listTruthWithIndexes.mkString("\n\n")
          }
        }
      )

    private def summarize(summaryTruth:List[TruthPiece[State]]):Translation[List[TruthPiece[State]], String] = {
      if(summaryTruth.isEmpty)
        Translated("But as Socrates we claim just to know that we know nothing.")
      else
        for {
          summarising <- Translated("Summarizing, this is what we know for sure:\n")
          summarized <- world.printer.translateFullScript(summaryTruth)
        } yield summarising ++ summarized
    }

    private def listTruthsWithIndexes(resultList:List[Truth[W]]):Translation[List[List[TruthPiece[State]]], List[String]] =
      Translator.traverse(resultList.zipWithIndex.map {
        case (truth, index) =>
          for {
            trIndex <- Translated[List[TruthPiece[State]], String](s"TruthEngine.Truth number ${index + 1}\n")
            trContent <- world.printer.translateFullScript(truth.truthPieces)
          } yield trIndex ++ trContent
      })
  }
}
