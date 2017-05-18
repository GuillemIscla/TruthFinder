import Worlds.GHE

import scala.io.Source

/**
  * Created by Iscle on 09/05/2017.
  */
object App {
  def main(args: Array[String]): Unit = {
    val fileName = """src\main\resources\GHE\Conversation8.txt"""
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
