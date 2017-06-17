import TruthEngine._
import Worlds._
import Language._
import TruthEngine.Truth.Truth

import scala.io.Source
import scala.reflect.ClassTag


object App {
  def main(args: Array[String]): Unit = {
    val world = GayClub
    val conversationNumber = 2
    val oneSolution:Boolean = false

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
    println(UserInterface.printTruth(world, world.findTruth(conversation), oneSolution))
    println("----------------")
    Thread.sleep(1000)
  }
}

object UserInterface{
  def printWorld[W <:World[W]](world:World[W]):String =
    s"Welcome to ${world.name} :D" + "\n" +
      world.description + "\n\n" +
      world.possibleWorldStates(None).map(ws => s"${ws.stringRef}: ${ws.description}").mkString("These are the world states:\n","\n","\n\n") +
      world.races().map(ws => s"${ws.stringRef}: ${ws.description}").mkString("These are the races in the world:\n","\n","\n\n")

  def printTruth[W <:World[W]](world:World[W], translatedListTruth:Translation[List[String], List[Truth]], oneSolution:Boolean):String = {
    translatedListTruth
      .newTranslation[String]
      .flatMap{
        truths =>
          val (header, footer) = headersFooters(world, truths)
          AppPrinter(world.truthPrinters, oneSolution, Some(header), Some(footer))
            .translate(mergeResultList(world, truths, oneSolution))
      } match {
        case Translated(result) =>
          result
        case NotTranslated(_) =>
          "Could not understand the TruthPieces in order to print them"
        case TranslationError(bad_script, error) =>
          s"There has been an error since the script '$bad_script' caused the error '$error'"
      }

  }

  private def mergeResultList[W <:World[W]](world:World[W], resultList:List[List[TruthPiece[State]]], oneSolution:Boolean):List[List[TruthPiece[State]]] =
    resultList.headOption.fold[List[List[TruthPiece[State]]]](List()){
      head =>
          if (oneSolution)
            List(resultList.tail.foldLeft(head) { case (tp1, tp2) => Truth.merge(tp1, tp2, world.customMerge) }.filter(_.state.isDefined))
          else
            resultList
      }

  private def headersFooters[W <:World[W]](world:World[W], truths:List[Truth]):(String, String) =
    (truths.headOption.fold("There are no possible truths")(_ => s"There ${if (truths.length == 1) "is" else "are"} ${truths.length} possible truth${if (truths.length == 1) "" else "s"}.\n"),
    world.extraDeductions(truths).mkString("\n", "\n", ""))

  case class AppPrinter[W <:World[W]](translatorCollection:List[Translator[List[TruthPiece[State]], String]], oneSolution:Boolean, header:Option[String] = None, footer:Option[String] = None) extends TextTranslator[List[TruthPiece[State]], String, String] {
    val scriptTag:ClassTag[List[TruthPiece[State]]] = implicitly[ClassTag[List[TruthPiece[State]]]]
    def generalCheck(listTranslated:List[String]):Translation[List[String], List[String]] = Translated(listTranslated)

    def formatResult(listTranslated:List[String]):String =
      header.fold("")(_ ++ "\n") ++
      listTranslated.headOption.fold("")(
        head =>
          if (oneSolution) {
            if (head.isEmpty) "But as Socrates we claim just to know that we know nothing."
            else "Summarizing, this is what we know for sure:\n" ++ head
          }
          else
            listTranslated.zipWithIndex.map { case (truth, index) => s"Truth number ${index + 1}\n" ++ truth }.mkString("\n\n")
      ) ++
      footer.fold("")(_ ++ "\n")

  }
}
