package TruthEngine

import scala.reflect.ClassTag

sealed trait Translation[Script, Result]{
  def map[B](f:Result=>B):Translation[Script, B]
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result]
  def newTranslation[NewResult](implicit tag: ClassTag[Result]):Translation[Result, NewResult]
  def translateFrom[NewScript](implicit tag: ClassTag[NewScript]):Translation[NewScript, Result]
  def translateTo[NewResult](implicit tag: ClassTag[NewResult]):Translation[Script, NewResult]
  def scriptMap[NewScript](f:Script=>NewScript)(implicit tag: ClassTag[NewScript]):Translation[NewScript, Result]
}

case class Translated[Script, Result](result:Result)(implicit scriptTag: ClassTag[Script]) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = Translated(f(result))
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = Translated(result)
  def newTranslation[NewResult](implicit tag: ClassTag[Result]):Translation[Result, NewResult] = NotTranslated(result)
  def translateFrom[NewScript](implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] = Translated(result)
  def translateTo[NewResult](implicit tag: ClassTag[NewResult]):Translation[Script, NewResult] =
    result match {
      case thisResult:NewResult =>
        Translated[Script, NewResult](thisResult)(scriptTag)
      case _ =>
        TranslationError(result.toString, TranslationError.changeTranslatatedDestination)
    }
  def scriptMap[NewScript](f:Script=>NewScript)(implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] = Translated[NewScript, Result](result)
}

case class NotTranslated[Script, Result](script:Script)(implicit scriptTag: ClassTag[Script]) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = NotTranslated(script)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = f(script).translateFrom[Script]
  def newTranslation[NewResult](implicit tag: ClassTag[Result]):Translation[Result, NewResult] = TranslationError(script.toString, TranslationError.notPreviouslytranslated)
  def translateFrom[NewScript](implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] =
    script match {
      case thisScript:NewScript =>
        NotTranslated(thisScript)
      case _ =>
        TranslationError(script.toString, TranslationError.changeNotTranslatedOrigin)
    }

  def translateTo[NewResult](implicit tag: ClassTag[NewResult]):Translation[Script, NewResult] = NotTranslated[Script, NewResult](script)(scriptTag)
  def scriptMap[NewScript](f:Script=>NewScript)(implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] = NotTranslated(f(script))
}

case class TranslationError[Script, Result](bad_script_string:String, error:String) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = TranslationError(bad_script_string, error)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = TranslationError(bad_script_string, error)
  def newTranslation[NewResult](implicit tag: ClassTag[Result]):Translation[Result, NewResult] = TranslationError(bad_script_string, error)
  def translateFrom[NewScript](implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] = TranslationError(bad_script_string, error)
  def translateTo[NewResult](implicit tag: ClassTag[NewResult]):Translation[Script, NewResult] = TranslationError(bad_script_string, error)
  def scriptMap[NewScript](f:Script=>NewScript)(implicit tag: ClassTag[NewScript]):Translation[NewScript, Result] = TranslationError(bad_script_string, error)
}

object TranslationError{
  val notPreviouslytranslated = "Was not previously translated"
  val changeTranslatatedDestination = "Tried to change translation destination with Translated script"
  val changeNotTranslatedOrigin = "Tried to change translation origin with NotTranslated script"
  def invalidCharacterError[Script, Result](invalidCharacter:String):TranslationError[Script, Result] =
    TranslationError(s"""Sentence where the character named: "$invalidCharacter" speaks or is mentioned""", s"""'$invalidCharacter' is an invalid character name""")
}

trait Translator[Script, Result] {
  def translate(script: Script): Translation[Script, Result]
}

trait TextTranslator[Script, PartialResult, FullResult] extends Translator[List[Script], FullResult] {
  implicit val scriptTag: ClassTag[Script]

  def translatorCollection: List[Translator[Script, PartialResult]]

  def generalCheck(listTranslated: List[PartialResult]): Translation[List[PartialResult], List[PartialResult]]

  def formatResult(listTranslated: List[PartialResult]): FullResult

  def translate(listScript: List[Script]): Translation[List[Script], FullResult] =
    listScript.map(operateTransCollection)
      .foldLeft[Translation[List[PartialResult], List[PartialResult]]](Translated(List())){
      case (Translated(list), Translated(newTranslation)) =>
        Translated(newTranslation :: list)
      case (notTranslated:NotTranslated[_, _], _) =>
        notTranslated
      case (_, NotTranslated(script)) =>
        TranslationError(script.toString, "Was not understood by the translators")
      case (translationError:TranslationError[_, _], _) =>
        translationError
      case (_, TranslationError(bad_script_string, error)) =>
        TranslationError(bad_script_string, error)
      }.map(_.reverse)
      .newTranslation[List[PartialResult]]
      .flatMap(generalCheck)
      .map(formatResult)
      .translateFrom[List[Script]]

  protected def operateTransCollection(script:Script):Translation[Script, PartialResult] =
    translatorCollection.foldLeft[Translation[Script, PartialResult]](NotTranslated(script)) {
      case (translating, newParser) =>
        translating.flatMap(newParser.translate)
    }
}