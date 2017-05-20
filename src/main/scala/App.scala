import Worlds._

import scala.io.Source


object App {
  def main(args: Array[String]): Unit = {
    val fileName = """src\main\resources\MagicForest\Conversation9.txt"""
    val conversation =
      Source.fromFile(fileName).getLines().toList
    val world = MagicForest
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
