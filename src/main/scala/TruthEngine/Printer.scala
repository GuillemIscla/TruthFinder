package TruthEngine

import Language._


trait TruthPiecePrinter extends Translator[TruthPiece[State], String]

object PrinterHelper {
  case object FinalPrinter extends TruthPiecePrinter {
    def translate(script: TruthPiece[State]): Translation[TruthPiece[State], String] =
      (script.reference, script.state) match {
        case (_: WorldAspectReference[_, _], Some(ws)) =>
          Translated(s"It is ${ws.stringRef}")
        case (Name(charName), Some(race)) =>
          Translated(s"$charName is ${race.stringRef}")
        case _ =>
          NotTranslated(script)
      }
  }
}