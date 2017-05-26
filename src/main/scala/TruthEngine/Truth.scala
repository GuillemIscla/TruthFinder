package TruthEngine

import TruthEngine.Language._

object Truth {
  type Truth = List[TruthPiece[State]]

  def merge(ltp1:Truth, ltp2:Truth, customMerge: (TruthPiece[State], TruthPiece[State]) => Option[TruthPiece[State]]):Truth = //Merge two truths that preserves the minimum set of certainty
    ltp1.zip(ltp2).map{
      case (tp1, tp2) =>
        customMerge(tp1, tp2).getOrElse(tp1.merge(tp2))
    }

  def compareTextAndTruth(world:World[_], truth: Truth, text: List[Sentence]):List[Truth]=
    nextAssumptions(world, truth) match {
      case Nil => //If there are no assumptions to make we know everything
        List(truth)
      case assumptions =>
        //We don't need to iterate nodes which already are contradictory. So we filter before the recursive call.
        assumptions
          .filter(ass => text.indices.forall(idx => sentenceCanBeSpoken(world, ass, text, idx)) && world.checkConsistency(ass))
          .flatMap(compareTextAndTruth(world, _, text))
    }

  def nextAssumptions(world:World[_], truth: Truth): List[Truth] =
    truth.find(_.state.isEmpty).fold(List.empty[Truth])(
      undefTp => {
        val index = truth.indexOf(undefTp)
        world.possibleTruthPieces(undefTp.reference).map {
          newAssumption =>
            truth.patch(index, Seq(newAssumption), 1)
        }
      }
    )

  def sentenceCanBeSpoken[S <: State](world:World[_], truth: Truth, text:List[Sentence], sentenceIndex: Int): Boolean = {
    val personality: Boolean => Boolean = //Function that the given character is going to use to talk sentences to us
      (for {
        truthPiece <- truth.find(_.reference == text(sentenceIndex).speaker)
        personality <- truthPiece.state collect { case race:Race => race.personality(truth, text, sentenceIndex)}
      } yield personality)
        .getOrElse(_ => true)
    //In case where we don't know the race of the character, anything
    //he says is sensible to us regardless of it is the truth or not

    text(sentenceIndex).compareWithTruth(truth)
      .fold(true)(personality)
    //If we don't know the boolean value of the sentence, whatever the character says offers no contradiction
    //If we know if the boolean value of the sentence, we check with the personality function if the character offer a contradiction
  }

}
