import scala.io.Source

/**
  * Created by Iscle on 09/05/2017.
  */
object App {
  def main(args: Array[String]): Unit = {

//    val sentences = List(
//      "A: I am Human",
//      "A: I am not Human",
//      "A: B is Human",
//      "A: B is not Human",
//      "A: Someone is Human",
//      "A: Someone is not Human",
//      "A: No one is Human",
//      "A: No one is not Human",
//      "A: There are at least 1 Human",
//      "A: There are at least 1 not Human",
//      "A: There are at least 1234 Human",
//      "A: There are at least 1234 not Human",
//      "A: There are exactly 1 Human",
//      "A: There are exactly 1 not Human",
//      "A: There are exactly 1234 Human",
//      "A: There are exactly 1234 not Human",
//      "MyName: There are exactly 1234 not Human",
//      "A: It is Night",
//      "A: It is not Night"
//    )

//    println(sentences.map(getSentence).mkString("\n"))

    val fileName = """src\main\resources\Conversation6.txt"""
    val conversation =
      Source.fromFile(fileName).getLines().toList

    println("----------------")
    println(fileName)
    println()
    println(conversation.mkString("\n"))
    println()
    println(GHE.findTruth(conversation, oneSolution = false))
    println("----------------")
    Thread.sleep(1000)
  }
}
