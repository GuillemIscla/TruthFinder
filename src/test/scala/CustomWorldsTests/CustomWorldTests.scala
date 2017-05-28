package CustomWorldsTests

import TruthEngine.Truth.Truth
import TruthEngine.{Translation, World}
import org.scalatest.{FunSuite, Matchers}

import scala.io.Source


trait CustomWorldTests[W <:World[W]] extends FunSuite with Matchers {

  def world:W
  def findTruthInConversation(conversationNumber:Int):Translation[List[String], List[Truth]] = {
    val fileName = s"src\\test\\resources\\${world.name}\\Conversation$conversationNumber.txt"
    world.findTruth(Source.fromFile(fileName).getLines().toList)
  }

}
