import Worlds.{GHE, Heaven}

import scala.io.Source

/**
  * Created by Iscle on 09/05/2017.
  */
object App {
  def main(args: Array[String]): Unit = {
    val fileName = """src\main\resources\GHE\Conversation2.txt"""
    val conversation =
      Source.fromFile(fileName).getLines().toList
    val world = GHE
    println("----------------")
    println(world)
    println("----------------")
    println(fileName)
    println()
    println(conversation.mkString("\n"))
    println()
    println("****RESULT****")
    println(world.findTruth(conversation, oneSolution = true))
    println("----------------")
    Thread.sleep(1000)
  }
}
