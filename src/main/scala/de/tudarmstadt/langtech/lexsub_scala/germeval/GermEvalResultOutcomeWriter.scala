package de.tudarmstadt.langtech.lexsub_scala.germeval
import de.tudarmstadt.langtech.scala_utilities._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import java.util.IllegalFormatException
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome


object GermEvalResultOutcomeWriter {
  def save(outcomes: Iterable[Outcome], outfile: String){
    new GermEvalResultOutcomeWriter(outcomes).save(outfile)
  }
}

class GermEvalResultOutcomeWriter(outcomes: Iterable[Outcome]){
  
  def formatLines: Iterable[String] = {
    outcomes.flatMap { 
      case Outcome(LexSubInstance(_, _, Some(GermEvalItem(sentence, gold))), scoredSubstitutes) =>
        scoredSubstitutes.map {
          case (subst, score) => 
            val entry = Seq(gold.target.word, gold.target.pos, gold.id, subst, score)
            entry.mkString("\t")
        }
      case _ => noGold
    }
  }
  
  def formatBest: Iterable[String] = {
    outcomes.map {
      case Outcome(LexSubInstance(_, _, Some(GermEvalItem(sentence, gold))), scoredSubstitutes) =>
        val left = gold.target.word + "." + gold.target.pos + " " + gold.id
        val best =  scoredSubstitutes.headOption.map(_._1).getOrElse("")
        left + " :: " + best
      case _ => noGold
    }
  }
  
  def formatOot: Iterable[String] = {
    outcomes.map {
      case Outcome(LexSubInstance(_, _, Some(GermEvalItem(sentence, gold))), scoredSubstitutes) =>
        val left = gold.target.word + "." + gold.target.pos + " " + gold.id
        val oot =  scoredSubstitutes.take(10).map(_._1).mkString(";")
        left + " ::: " + oot
      case _ => noGold
    }
  }
  
  def formatRank: Iterable[String] = {
    outcomes.map {
      case Outcome(LexSubInstance(_, _, Some(GermEvalItem(sentence, gold))), scoredSubstitutes) =>
        val left = gold.target.word + "." + gold.target.pos + " " + gold.id
        val rank =  scoredSubstitutes.map(_._1).mkString(";")
        left + " :::: " + rank
      case _ => noGold
    }
  }
  
  def save(outfile: String){
    io.write(outfile, formatLines.mkString("\n"))
    io.write(outfile + ".best", formatBest.mkString("\n"))
    io.write(outfile + ".oot", formatOot.mkString("\n"))
    io.write(outfile + ".rank", formatRank.mkString("\n"))
    
    val prettyPrinted = new GermEvalResultOutcomeReader(outcomes.map(_.lexSubInstance).toSeq).prettyPrint(outfile)
    io.write(outfile + ".prettyprint.txt", prettyPrinted)
  }
  
  private def noGold = throw new IllegalArgumentException("outcome has no gold data")
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
  
  
  /** Pretty-prints an "instances" file (as used by lexsub or lexsub-scala) and outputs a human-readable pretty print version
   *  @param outputHtml enables HTML output instead of plaintext */
  def prettyPrint(filename: String, maxExpansions: Int = 5, context: (Int, Int) = (5, 5), outputHtml: Boolean = false): String = {
    val parsed = parse(filename)
    val output = new StringBuffer
    
    val tables = for((item, allOutcomes) <- parsed) yield {
      val outcomes = allOutcomes.take(maxExpansions)
      val words1 = item.sentence.tokens.map(_.word)
      val (left, right) = context
      val words = collections.context(left, right)(words1, item.headIndex).map(_.getOrElse("")).toVector
      val emptyWords = words.map(_.replaceAll(".", " "))
      val empty = Vector.fill(words.length)("")
      
      val correctSet = allOutcomes.map(_._1).toSet.intersect(item.gold.get.gold.substitutions.map(_._1).toSet)
      def mkBold(subst: String) = {
        if(correctSet.contains(subst)) <b>{subst}</b>
        else subst
      }
      def mkUpper(subst: String) = if(correctSet.contains(subst)) "* " + subst.toUpperCase else subst
      
      def mkHtmlTable(outcomes: List[(String, Any)]) = outcomes.map { case (s, score) => 
        empty.updated(left, s).updated(left + 1, score.toString)
      }
      
      def mkPrintTable(outcomes: List[(String, Any)]) = outcomes.map { case (s, score) => 
        emptyWords.updated(left, mkUpper(s) + "   " + score.toString.take(4))
      }
      
      val mkTable = mkPrintTable _
      
      val substs = mkTable(outcomes)
      val goldlines = mkTable(item.gold.get.gold.substitutions.sortBy(_._2))
      
      val table =  (goldlines ::: (words :: substs))
      
      val name = "Id: " + item.gold.get.sentence.id
      val main = table.map(_.mkString(" "))
      val lines = main ::: List( /*"Error class: TODO",*/ "", "")
      
      output append lines.mkString("\n")
      
      def elem(e: String) = <td>{mkBold(e)}</td>
      def tline(row: Vector[String]) = <tr>{row.map(elem)}</tr>
      def html(table: List[Vector[String]]) = {
        <table>{table.map(tline)}</table>
      }

      html(table)
    }
    
    val htmlResult = 
    <html>{ 
      tables.map(table => 
      <div>
      <hr></hr>{table}
      </div>)}
    </html>
    
    if(outputHtml) htmlResult.toString else  output.toString
  }
}