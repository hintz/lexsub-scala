package de.tudarmstadt.langtech.lexsub_scala.germeval
import de.tudarmstadt.langtech.lexsub_scala.utility
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


class GermEvalResultOutcomeReader(val gold: Seq[LexSubInstance]) {
  val idLookup = gold.map(g => (g.gold.get.sentence.id, g)).toMap
  
  def parse(filename: String) = {
    val lines = io.lines(filename)
    val foo = lines.map(_.split("\t")).toList
    val byId = foo.filter(_.length == 5).groupBy(_(2))
    val result = byId.toList.flatMap { case (id, lines) => 
      idLookup.get(id).map { germEvalGold =>
        val outcomes = lines.map(line => line.takeRight(2) match { case Array(word, score) => (word, score.toDouble)})
        (germEvalGold, outcomes)
      }
    }
    result.sortBy(_._1.gold.get.sentence.id)
  }
  
  
  def prettyPrint(filename: String, maxN: Int = 10){
    val parsed = parse(filename)
    for((item, outcomes) <- parsed){
      val words = item.sentence.tokens.map(_.word)
      val emptyWords = words.map(_.replaceAll(".", " "))
      val substs = outcomes.take(maxN).map { case (s, score) => 
        emptyWords.updated(item.headIndex, "%s   %.2f".format(s, score))
      }
      val name = "Id: " + item.gold.get.sentence.id
      val goldSubst = "Gold: " + item.gold.get.gold.substitutionWords.mkString(", ")
      val lines = goldSubst :: (words :: substs).map(_.mkString(" ")) ::: List("Error class: ", "", "")
      lines.foreach(println)
    }
  }
}