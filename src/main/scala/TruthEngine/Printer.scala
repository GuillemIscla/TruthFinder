package TruthEngine

import Language._

trait Printer extends Translator[TruthPiece[State], String, String]{
  def script_general_check(resultList:List[String]): Translation[List[String], String] = Translated(resultList.mkString("\n"))
  def translateScriptSentence(raw_script_sentence: TruthPiece[State]): Translation[TruthPiece[State], String]
}

case class PrinterCollection(translatorList:List[Printer]) extends Printer with TranslatorCollection[TruthPiece[State], String, String]

object PrinterHelper {
  case object FinalPrinter extends Printer {
    def translateScriptSentence(raw_script_sentence: TruthPiece[State]): Translation[TruthPiece[State], String] =
      (raw_script_sentence.reference, raw_script_sentence.state) match {
        case (_: WorldAspectReference[_, _], Some(ws)) =>
          Translated(s"It is $ws")
        case (Name(charName), Some(race)) =>
          Translated(s"$charName is $race")
        case _ =>
          NotTranslated(raw_script_sentence)
      }
  }
}