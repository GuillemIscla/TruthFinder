/**
  * Created by Iscle on 09/05/2017.
  */
object App {
  def main(args: Array[String]): Unit = {

    val w:World[GHE] = GHE
    val text:String = ""
    val oneSolution = true

    println(w.findTruth(text, oneSolution))
  }
}
