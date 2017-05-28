package TruthFinderTests

import TruthEngine._
import org.scalatest.{FunSuite, Matchers}

trait TranslationTests extends FunSuite with Matchers

class TranslatedTests extends TranslationTests {
  test("Translated should perform map") {
    Translated("one").map(r => r + r) should be (Translated("oneone"))
  }

  test("Translated should not do anything when perform flatMap") {
    Translated[Int, String]("one").flatMap(_ => Translated("one")) should be (Translated("one"))
    Translated[Int, String]("one").flatMap(_ => NotTranslated(1)) should be (Translated("one"))
    Translated[Int, String]("one").flatMap(result => TranslationError(result.toString, "Had an error")) should be (Translated("one"))
  }

  test("Translated should perform newTranslation") {
    Translated("one").newTranslation[Boolean] should be (NotTranslated("one"))
  }

  test("Translated should perform translateFrom another origin") {
    Translated("one").translateFrom[Boolean] should be (Translated("one"))
  }

  test("Translated should perform translateFrom same origin") {
    Translated("one").translateFrom[Int] should be (Translated("one"))
  }

  test("Translated should go to error when perform translateTo another destination") {
    Translated("one").translateTo[Boolean] should be (TranslationError("one", TranslationError.changeTranslatatedDestination))
  }

  test("Translated should not do anything when perform translateTo same destination") {
    Translated("one").translateTo[String] should be (Translated("one"))
  }
}

class NotTranslatedTests extends TranslationTests {
  test("NotTranslated should not do anything when perform map") {
    NotTranslated[Int, String](1).map(r => r + r) should be (NotTranslated(1))
  }

  test("NotTranslated should perform flatMap") {
    NotTranslated[Int, String](1).flatMap(_ => Translated("one")) should be (Translated("one"))
    NotTranslated[Int, String](1).flatMap(_ => NotTranslated(1)) should be (NotTranslated(1))
    NotTranslated[Int, String](1).flatMap(result => TranslationError(result.toString, "Had an error")) should be (TranslationError("1", "Had an error"))
  }

  test("NotTranslated should go to error when perform newTranslation") {
    NotTranslated(1).newTranslation[Boolean] should be (TranslationError("1", TranslationError.notPreviouslytranslated))
    NotTranslated(1).newTranslation[Int] should be (TranslationError("1", TranslationError.notPreviouslytranslated))
  }

  test("NotTranslated should go to error when perform translateFrom another origin") {
    NotTranslated(1).translateFrom[Boolean] should be (TranslationError("1", TranslationError.changeNotTranslatedOrigin))
  }

  test("NotTranslated should not do anything when perform translateFrom same origin") {
    NotTranslated(1).translateFrom[Int] should be (NotTranslated(1))
  }

  test("NotTranslated should perform translateTo another destination") {
    NotTranslated(1).translateTo[Boolean] should be (NotTranslated(1))
  }

  test("NotTranslated should perform translateTo same destination") {
    NotTranslated(1).translateTo[String] should be (NotTranslated(1))
  }
}

class TranslationErrorTests extends TranslationTests {
  test("TranslationError should not do anything when perform map") {
    TranslationError[Int, String]("bad_script_string", "error").map(r => r + r) should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform flatMap") {
    TranslationError[Int, String]("bad_script_string", "error").flatMap(_ => Translated("one")) should be (TranslationError("bad_script_string", "error"))
    TranslationError[Int, String]("bad_script_string", "error").flatMap(_ => NotTranslated(1)) should be (TranslationError("bad_script_string", "error"))
    TranslationError[Int, String]("bad_script_string", "error").flatMap(result => TranslationError(result.toString, "Had an error")) should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform newTranslation") {
    TranslationError[Int, String]("bad_script_string", "error").newTranslation[Boolean] should be (TranslationError("bad_script_string", "error"))
    TranslationError[Int, String]("bad_script_string", "error").newTranslation[Int] should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform translateFrom another origin") {
    TranslationError[Int, String]("bad_script_string", "error").translateFrom[Boolean] should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform translateFrom same origin") {
    TranslationError[Int, String]("bad_script_string", "error").translateFrom[Int] should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform translateTo another destination") {
    TranslationError[Int, String]("bad_script_string", "error").translateTo[Boolean] should be (TranslationError("bad_script_string", "error"))
  }

  test("TranslationError should not do anything when perform translateTo same destination") {
    TranslationError[Int, String]("bad_script_string", "error").translateTo[String] should be (TranslationError("bad_script_string", "error"))
  }
}