## Truth Finder

The truth finder is a logic engine that solves logic games such as the following:

In this world there are 3 races:
- **Gods:** They always speak the truth
- **Humans:** They speak the truth during the day but they lie at night
- **Evil:** They always lie

In this world there are two states: **Day** and **Night**

Find all the information possible in the input conversation:

INPUT:
```
    A: I am Evil
```

SOLUTION:
```
    A is Human
    It is Night
```

**The _Translation_ Monad**

The most complex part was not the logic in the engine but the parsing of the text and printing the result. As the engine should be versatile and work with different situation (that is, not only with Gods, Humans and Evils world) each World definition can provide information to the engine parser (and there are up to 7 different worlds defined).

To combine different parsers the Either monad was proven to be insufficient since given a script to translate and a parser there can be 3 different scenarios: 
1) Succesfully parsed 
2) The script has not been parsed yet
3) Error happened during translation

The proper piping of these parsers in a monadic way was not possible with the most common monads and so, the Translation monad was defined, with three possible forms: **Translated**, **NotTranslated** and **TranslationError**. This is what the basic operations model: 
- The map operation models any map transformation to do in the translated text (but does not transform the script, just as map does not affect Left in the Either)
- The flatMap operation models an attempt to translate the script. That is unvariable with Translated or NotTranslated forms.

An intuitive way to see monads is "entities that potentially can deliver a value" and so, the translation monad will deliver a translation when the suitable parser tries to translate it (that is, tries to flatMap it)

A naive coding of the three forms follows. For a complete (and correct) coding, you can look at the source code.
```
trait Translation[Script, Result]
case class Translated[Script, Result](result:Result)extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = Translated(f(result))
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = Translated(result)
}

case class NotTranslated[Script, Result](script:Script) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = NotTranslated(script)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = f(script)
}

case class TranslationError[Script, Result](bad_script_string:String, error:String) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = TranslationError(bad_script_string, error)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = TranslationError(bad_script_string, error)
}
```

To properly use the translation monad, the project uses Translators. A Translator contains the translation logic and when using translate function any of the following could happen:
1) Identifies the script as for him to translate and translates it to the result type. 
2) Identifies the script as for him to translate but produces an error when translating.
3) Identifies the script as not for him to translate.

This is another naive example of how to use the translator assuming that we have natural language parsers from several languages (codified as String) onto some type (GrammarObject) that is meaninful to the rest of the program.

```
trait Translator[Script, Result]{
  def language: String
  def translate(script:Script):Translation[Script, Result]
}

trait GrammarObject

case class Person(name:String) extends GrammarObject

case object EnglishTranslator extends Translator[String, GrammarObject]{
  val language = "English"
  def translate(script:String):Translation[String, GrammarObject] = ???
}

case object FrenchTranslator extends Translator[String, GrammarObject]{
  val language = "French"
  def translate(script:String):Translation[String, GrammarObject] = ???
}


object Exercise extends App {

    val scriptA:String = "Je m'appelle Guillem" //Script in correct French
    val scriptToTranslateA = NotTranslated[String, GrammarObject](scriptA)


    //Won't be translated, and translation1A will result in NotTranslated(scriptA)
    val translation1A:Translation[String, GrammarObject] =
      scriptToTranslateA.flatMap(EnglishTranslator.translate)

    //Will be translated, and translation2A will result in Translated(Person("Guillem"))
    val translation2A:Translation[String, GrammarObject] =
      translation1A.flatMap(FrenchTranslator.translate)


    val scriptB:String = "Mai name is Guillem" //Script in English but with grammar errors
    val scriptToTranslateB = NotTranslated[String, GrammarObject](scriptB)

    //Won't be translated, and translation1B will result in TranslationError("Mai", "Mai is not correct in English")
    //This is the case when the translator can tell he is the one to translate the text, however
    //still produces error when translating
    val translation1B:Translation[String, GrammarObject] =
      scriptToTranslateB.flatMap(EnglishTranslator.translate)

    //Will keep the error found in translation1B and so translation2B will still 
    //be TranslationError("Mai", "Mai is not correct in English")
    val translation2B:Translation[String, GrammarObject] =
      translation1B.flatMap(FrenchTranslator.translate)
}
      
```

The power of adding custom parsers can be checked in the code of the world named MagicForest inside the project. In this world the world state can be either **Day** or **Night**, but as well can be either or be **WhiteMagic** or **BlackMagic**.
- So to express that the state is Day the sentence _"It is Day"_ is good enough for plain english. 
- However _"It is WhiteMagic"_ sounds pretty rusty. Luckily, the custom parsers provided by this world code can understand that this condition is expressed with the sentence _"There is WhiteMagic around"_.
