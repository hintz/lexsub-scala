package de.tudarmstadt.langtech.lexsub_scala.germeval
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import java.util.IllegalFormatException
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome

class GermEvalResultOutcomeWriter(outcomes: Iterable[Outcome]){

  def formatLines: Iterable[String] = {
    outcomes.flatMap { 
      case Outcome(LexSubInstance(_, _, Some(GermEvalItem(sentence, gold))), scoredSubstitutes) =>
        scoredSubstitutes.map {
          case (subst, score) => 
            val entry = Seq(gold.target.word, gold.target.pos, gold.id, subst, score)
            entry.mkString("\t")
        }
      case _ => throw new IllegalArgumentException("outcome has no gold data")
    }
  }
  
  def save(outfile: String){
    io.write(outfile, formatLines.mkString("\n"))
  }
}