## Truth Finder

The truth finder is a logic engine that solves logic games such as the following:

In this world there are 3 races:
- **Gods:** They always speak the truth
- **Humans:** They speak the truth during the day but they lie at night
- **Evil:** They always lie

Find all the information possible in the input conversation:

INPUT:
```
    A: I am Evil
```

SOLUTION:
```
    A is Human
    It is night
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

case class NotTranslated[Script, Result](script:Script)(implicit scriptTag: ClassTag[Script]) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = NotTranslated(script)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = f(script)
}

case class TranslationError[Script, Result](bad_script_string:String, error:String) extends Translation[Script, Result] {
  def map[B](f: Result => B): Translation[Script, B] = TranslationError(bad_script_string, error)
  def flatMap[B](f:Script=>Translation[B, Result]):Translation[Script, Result] = TranslationError(bad_script_string, error)
}
```
The power of adding custom parsers can be checked in the code world named MagicForest. In this world the world state can be either **Day** or **Night**, but as well can be either or be **WhiteMagic** or **BlackMagic**.
- So to express that the state is Day the sentence "It is Day" is good enough for plain english. 
- However "It is WhiteMagic" sounds pretty rusty. Luckily, the custom parsers provided by this world code can understand that this is expressed as "There is WhiteMagic around".
