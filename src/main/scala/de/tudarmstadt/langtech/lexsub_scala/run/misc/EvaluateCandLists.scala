package de.tudarmstadt.langtech.lexsub_scala.run.misc

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.compute
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalGold
import de.tudarmstadt.langtech.lexsub_scala.reader.GoldItem
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings
import de.tudarmstadt.langtech.lexsub_scala.types.PRResult
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.English
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.German
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.Italian
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate

case class EvalResults(pr: PRResult)

object EvaluateCandLists extends App {
  
  def evaluate(gold: List[GoldItem], candidates: String => Seq[Candidate]) = {
        val subresults = for(item <- gold) yield {
          val gold = item.substitutionWords
          val retrieved = candidates(item.target.word).map(_.replacement)
          val tp = gold.count(g => retrieved.contains(g))
          val fn = gold.count(g => !retrieved.contains(g))
          val tpAlso = retrieved.count(r => gold.contains(r))
          val fp = retrieved.count(r => !gold.contains(r))
          //assert(tp == tpAlso)
          assert(tp + fn == gold.length)
          PRResult(tp, fp, fn, None)
        }
        subresults.reduce(_ + _)
  }
  
  
  val languages = List(English, German, Italian)
  
  val subresults = for(lang <- languages) yield {
    val gold = new SemEvalGold(lang.testGoldfile)
    println(lang)
    val result = evaluate(gold.items, lang.candidates.get)
    println(result)
    result
  }
  
  println(subresults.reduce(_ + _))
  
  

}