package de.tudarmstadt.langtech.lexsub_scala.germeval
import de.tudarmstadt.langtech.lexsub_scala.utility.io

class GermEvalResultOutcomeWriter(outcomes: Iterable[(GermEvalItem, Seq[(String, Double)])]){

  def formatLines: Iterable[String] = {
    outcomes.flatMap { 
      case (GermEvalItem(sentence, gold), scoredSubstitutes) =>
        scoredSubstitutes.map {
          case (subst, score) => 
            val entry = Seq(gold.target.word, gold.target.pos, gold.id, subst, score)
            entry.mkString("\t")
        }
    }
  }
  
  def save(outfile: String){
    io.write(outfile, formatLines.mkString("\n"))
  }
}