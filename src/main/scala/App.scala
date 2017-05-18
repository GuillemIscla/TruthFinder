import scala.io.Source


object App {
  def main(args: Array[String]): Unit = {
    val fileName = """src\main\resources\Hell\Conversation1.txt"""
    val conversation =
      Source.fromFile(fileName).getLines().toList
    val world = Hell
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
