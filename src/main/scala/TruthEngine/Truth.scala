package TruthEngine

import TruthEngine.Language._

case class Truth[W <: World[W]](world:W, truthPieces:List[TruthPiece[State]]) {

  def nextAssumptions(): List[Truth[W]] =
    truthPieces.find(_.state.isEmpty).fold(List.empty[Truth[W]])(
      undefTp => {
        val index = truthPieces.indexOf(undefTp)
        world.possibleTruthPieces(undefTp.reference).map {
          newAssumption =>
            Truth(world, truthPieces.patch(index, Seq(newAssumption), 1))
        }
      }
    )


  def conversationCanTakePlace[S <: State](text:List[Sentence], sentenceIndex: Int): Boolean = {
    val personality: Boolean => Boolean = //Function that the given character is going to use to talk sentences to us
      (for {
        truthPiece <- truthPieces.find(_.reference == text(sentenceIndex).speaker)
        personality <- truthPiece.state collect { case race:Race => race.personality(this, text, sentenceIndex)}
      } yield personality)
        .getOrElse(_ => true)
    //In case where we don't know the race of the character, anything
    //he says is sensible to us regardless of it is the truth or not

    text(sentenceIndex).compareWithTruth(this)
      .fold(true)(personality)
    //If we don't know the boolean value of the sentence, whatever the character says offers no contradiction
    //If we know if the boolean value of the sentence, we check with the personality function if the character offer a contradiction
  }
}

object Truth {
  def merge[W <: World[W]](t1:Truth[W], t2:Truth[W], customMerge: (TruthPiece[State], TruthPiece[State]) => Option[TruthPiece[State]]):Truth[W] = //Merge two truths that preserves the minimum set of certainty
    Truth(t1.world, t1.truthPieces.zip(t2.truthPieces).map{
      case (tp1, tp2) =>
        customMerge(tp1, tp2).getOrElse(tp1.merge(tp2))
    })

  def compareTextAndTruth[W <: World[W]](truth: Truth[W], text: List[Sentence]):List[Truth[W]]=
    truth.nextAssumptions() match {
      case Nil => //If there are no assumptions to make we know everything
        List(truth)
      case assumptions =>
        //We don't need to iterate nodes which already are contradictory. So we filter and recursive call.
        assumptions
          .filter(ass => text.indices.forall(idx => ass.conversationCanTakePlace(text, idx)) && truth.world.checkWorldState(ass))
          .flatMap(compareTextAndTruth(_, text))
    }
}
