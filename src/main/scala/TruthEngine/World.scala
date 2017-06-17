package TruthEngine

import TruthEngine.Language._
import TruthEngine.ParserHelper._
import TruthEngine.PrinterHelper._
import TruthEngine.Truth.Truth

import scala.reflect.ClassTag

trait World[W <: World[W]] {
  val worldInstance:W
  val name:String
  val description:String
  def possibleWorldStates(war:Option[WorldAspectReference[W, WorldState[W]]] = None, text:List[Sentence] = List()):List[WorldState[W]]
  def possibleWorldAspects(ws:Option[WorldState[W]] = None, text:List[Sentence] = List()):List[WorldAspectReference[W, WorldState[W]]]
  def checkConsistency(truth:Truth):Boolean
  def customMerge(tp1:TruthPiece[State], tp2:TruthPiece[State]): Option[TruthPiece[State]] = None
  def extraDeductions(truths:List[Truth]):List[String] = List()
  def races(text:List[Sentence] = List()):List[Race]
  lazy val customParsers:List[LineParser[W]] = List()
  lazy val customPrinters:List[TruthPiecePrinter] = List()

  lazy val truthPrinters:List[Translator[Truth, String]] = List(TruthPrinter(customPrinters ++ List(FinalPrinter)))

  case class TruthPrinter[W <:World[W]](translatorCollection:List[Translator[TruthPiece[State], String]]) extends TextTranslator[TruthPiece[State], String, String] {
    val scriptTag:ClassTag[TruthPiece[State]] = implicitly[ClassTag[TruthPiece[State]]]
    def generalCheck(listTranslated: List[String]): Translation[List[String], List[String]] = Translated(listTranslated)
    def formatResult(listTranslated: List[String]): String = listTranslated.mkString("\n")
  }

  def findTruth(raw_text:List[String]):Translation[List[String], List[Truth]] =
    TextParser(worldInstance,  List(RegularWorldStateParser(worldInstance), GenericParser(worldInstance)) ++ customParsers).translate(raw_text).map {
      case (socratesTruth, text) =>
         Truth.compareTextAndTruth(this, socratesTruth, text)
    }

  def possibleTruthPieces(s:Reference[_], text:List[Sentence]):List[TruthPiece[State]] = s match {
    case war:WorldAspectReference[W, WorldState[W]]@unchecked =>
      possibleWorldStates(Some(war), text).map(ws => WorldAspect(war, Some(ws)))
    case name:Name =>
      races(text).map(r => Character(name, Some(r)))
    case _ =>
      List()
  }

  protected def findState(truth:Truth, reference:Reference[State]):Option[State] =
    (for {
      truthPiece <- truth.collect{case tp if tp.reference == reference => tp}
      truthState <- truthPiece.state
    } yield truthState).headOption

  protected def findCharsOfRace(truth:Truth, race:Race):List[Name] =
    truth.collect{case ch:Character if ch.state.contains(race) => ch.reference}

  protected def traverseTruthResults(truths:List[Truth]):List[(Reference[State], List[State])] =
    truths.headOption.fold(List.empty[(Reference[State], List[Option[State]])])(
      head =>
        truths.tail.foldLeft[List[(Reference[State], List[Option[State]])]](
          head.map{tp => (tp.reference, List(tp.state))}
        ){
          case (list, truth) =>
            list.zip(truth.map{tp => (tp.reference, tp.state)}).map{
              case (listTp, newTp) =>
                (listTp._1, newTp._2 :: listTp._2)
            }
        }
    ).map{case (ref, listOptionState) => (ref, listOptionState.flatten)}
}
