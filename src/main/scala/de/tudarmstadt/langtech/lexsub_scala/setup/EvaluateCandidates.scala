package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germeval.GoldItem
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate

case class PRResult(val tp: Int, val fp: Int, val fn: Int, val tn: Option[Int]) {
  def retrieved = tp + fp
  def relevant = tp + fn
  def +(other: PRResult) = PRResult(tp + other.tp, fp + other.fp, fn + other.fn, for(a <- tn; b <- other.tn) yield a + b)
  def precision: Double = tp / retrieved.toDouble
  def recall: Double = tp / relevant.toDouble
  def fmeasure: Double = utility.fmeasure(precision, recall)
  override def toString = "R=%.2f P=%.2f F1=%.2f".format(recall, precision, fmeasure)
}

case class EvalResults(pr: PRResult)

object EvaluateCandidates extends App {
  
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

  val gold = new GermEvalReader("AIPHES_Data/GermEval2015", "train-dataset").gold

  val germanetCandidates = new CandidateFile("AIPHES_Data/LexSub/candidates/germanet_candidates")
  val germanetCandidatesHy = new CandidateFile("AIPHES_Data/LexSub/candidates/germanet_candidates-hy")
  val germanetCandidatesHyHo = new CandidateFile("AIPHES_Data/LexSub/candidates/germanet_candidates-hy-ho")
  val germanetGermevalAll = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_germanet.tsv", true)

  val duden = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_duden.tsv", true)
  val woxikon = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_woxikon.tsv", true)
  val wortschatzSyn = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_wortschatz.tsv", true)

  val CandidateLists = List(
    germanetCandidates, germanetCandidatesHy, germanetCandidatesHyHo, germanetGermevalAll,
    duden, woxikon, wortschatzSyn
  )
  
  val masterlist = new JoinedCandidates(duden, woxikon, wortschatzSyn, germanetGermevalAll)
  //masterlist.save("germeval_masterlist.tsv")
  
  /*
  // Writes outcomes.csv for Machine Learning eval
  val goldmap = gold.items.groupBy(_.targetWord).mapValues(_.flatMap(_.substitutionWords).toSet)
  def outcome(target: String, replacement: String): String = if(goldmap.getOrElse(target, Set.empty).contains(replacement)) "GOOD" else "BAD"
  val lines = masterlist.formatLines.map { line =>
    val (target :: pos :: replacement :: relations :: Nil) = line.split("\t").toList.asInstanceOf[List[String]]
    relations + "\t" + outcome(target, replacement)
  }
  io.write("outcomes.csv", lines.mkString("\n"))
  */
  
  val Joined = List(
		  new JoinedCandidates(duden, woxikon, wortschatzSyn),
      masterlist
   )
   
  val Filtered = duden.filteredByAllRelations ++ woxikon.filteredByAllRelations ++ wortschatzSyn.filteredByAllRelations
   
  val Evaluate: Seq[CandidateList] = CandidateLists ++ Joined //++ Filtered.sortBy(_.toString)
  for (candidateList <- Evaluate) {
    println(candidateList)
    println(evaluate(gold.items, candidateList.get))
  }
  
  /*
  // writes sample word
  for (c <- CandidateLists) {
    println(c) 
    c.get("glaubhaft").map(_.replacement) foreach println
  }*/
  
}