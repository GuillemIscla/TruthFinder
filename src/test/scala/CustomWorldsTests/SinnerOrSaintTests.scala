package CustomWorldsTests

import TruthEngine.Language._
import TruthEngine.{Translated, TranslationError}
import Worlds.SinnerOrSaint
import Worlds.SinnerOrSaint._

class SinnerOrSaintTests extends CustomWorldTests[SinnerOrSaint]{
  def world:SinnerOrSaint = SinnerOrSaint

  test("Conversations in SinnerOrSaint"){
    findTruthInConversation(2) should be (Translated(List(List(WorldAspect(SuperNaturalPresenceReference,Some(DaemonPresence)), Character(Name("A"),Some(Human(Some(Saint))))), List(WorldAspect(SuperNaturalPresenceReference,Some(DaemonPresence)), Character(Name("A"),Some(Human(Some(Undecided))))))))
  }

  test("Custom parsing in SinnerOrSaint"){
    SinnerOrSaintParser.translate("A: It feels there is some WeirdThing") should be (TranslationError("WeirdThing", "WeirdThing is not a valid presence in the SinnerOrSaint world"))
    SinnerOrSaintParser.translate("A: It feels there is some DaemonPresence") should be (Translated(Sentence(Name("A"), SuperNaturalPresenceReference, None, StateDirectObject(DaemonPresence), directObjectAffirmation = true)))
  }

  test("Custom print in SinnerOrSaint"){
    SinnerOrSaintPrinter.translate(Character(Name("A"), Some(Human(Some(Saint))))) should be (Translated("A is Human and he is a holy Saint :D"))
    SinnerOrSaintPrinter.translate(Character(Name("A"), Some(Human(Some(Undecided))))) should be (Translated("A is Human and he is Undecided :O"))
    SinnerOrSaintPrinter.translate(Character(Name("A"), Some(Human(Some(Sinner))))) should be (Translated("A is Human and he is a shameful Sinner :'("))
    SinnerOrSaintPrinter.translate(Character(Name("A"), Some(Human(None)))) should be (Translated("A is Human but we don't know his status (¿?)"))
    SinnerOrSaintPrinter.translate(WorldAspect(SuperNaturalPresenceReference, Some(AngelPresence))) should be (Translated("There is some AngelPresence"))
    SinnerOrSaintPrinter.translate(WorldAspect(SuperNaturalPresenceReference, Some(DaemonPresence))) should be (Translated("There is some DaemonPresence"))
  }

  test("Custom merge in SinnerOrSaint"){
    val saint = Character(Name("A"), Some(Human(Some(Saint))))
    val sinner = Character(Name("A"), Some(Human(Some(Sinner))))
    val undecided = Character(Name("A"), Some(Human(Some(Undecided))))
    val undefined = Character(Name("A"), Some(Human(None)))
    val angel = Character(Name("A"), Some(Angel))

    SinnerOrSaint.customMerge(saint, saint) should be (Some(saint))
    SinnerOrSaint.customMerge(sinner, sinner) should be (Some(sinner))
    SinnerOrSaint.customMerge(undecided, undecided) should be (Some(undecided))
    SinnerOrSaint.customMerge(saint, sinner) should be (Some(undefined))
    SinnerOrSaint.customMerge(saint, undefined) should be (Some(undefined))
    SinnerOrSaint.customMerge(saint, angel) should be (None)
  }
}
