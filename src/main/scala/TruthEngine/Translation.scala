package TruthEngine

sealed trait Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B]
  def flatMap[B](f: Result => Translation[_, B]): Translation[Script, B]
}

case class Translated[Script, Result](result:Result) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = Translated(f(result))
  def flatMap[B](f: Result => Translation[_, B]): Translation[Script, B] =
    f(result) match {
      case Translated(_result) =>
        Translated(_result)
      case NotTranslated(script) =>
        TranslationError(script.toString, s"Was not previously translated")
      case TranslationError(bad_script, error) =>
        TranslationError(bad_script, error)
    }
}

case class NotTranslated[Script, Result](script:Script) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = NotTranslated(script)
  def flatMap[B](f: Result => Translation[_, B]): Translation[Script, B] = NotTranslated(script)
}

case class TranslationError[Script, Result](bad_script:String, error:String) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = TranslationError(bad_script, error)
  def flatMap[B](f: Result => Translation[_, B]): Translation[Script, B] = TranslationError(bad_script, error)
}

object Translator {
  def traverse[Script, Result](translationList:List[Translation[Script, Result]]):Translation[List[Script], List[Result]] =
    translationList
        .foldLeft[Translation[List[Script], List[Result]]](Translated(List())){
          case(Translated(list), Translated(sentence)) =>
            Translated(sentence :: list)
          case(notParsedOrError, _:Translated[Script, Result]) =>
            notParsedOrError
          case(_, NotTranslated(script)) =>
            TranslationError(script.toString, "This original script was not understood")
          case(_, TranslationError(bad_script, error)) =>
            TranslationError(bad_script, error)
        }.map(_.reverse)
}

trait Translator[Script, PartialResult, FullResult] {
  def translateScriptSentence(raw_script_sentence: Script): Translation[Script, PartialResult]
  def script_general_check(resultList:List[PartialResult]): Translation[List[PartialResult], FullResult]
  def translateFullScript(raw_script:List[Script]):Translation[List[Script], FullResult] =
    Translator.traverse(raw_script.map(translateScriptSentence))
      .flatMap(script_general_check)
}

trait TranslatorCollection[Script, PartialResult, FullResult] extends Translator[Script, PartialResult, FullResult] {
  val translatorList:List[Translator[Script, PartialResult, FullResult]]

  def translateScriptSentence(raw_script_sentence: Script): Translation[Script, PartialResult] =
    translatorList.foldLeft[Translation[Script, PartialResult]](NotTranslated(raw_script_sentence)){
      case (NotTranslated(notTranslated), nextParser) =>
        nextParser.translateScriptSentence(notTranslated)
      case (parsedOrError, _) =>
        parsedOrError
    }
}