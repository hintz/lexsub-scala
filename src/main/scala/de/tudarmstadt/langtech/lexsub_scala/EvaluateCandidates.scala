package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.candidates.SimpleCandidateFile
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germeval.GoldItem
import de.tudarmstadt.langtech.lexsub_scala.candidates.Candidate


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
          val tp = retrieved.count(r => gold.contains(r))
          val fp = retrieved.count(r => !gold.contains(r))
          val fn = gold.count(g => !retrieved.contains(g))
          PRResult(tp, fp, fn, None)
        }
        subresults.reduce(_ + _)
  }

  val gold = new GermEvalReader("AIPHES_Data/GermEval2015", "train-dataset").gold
  val CandidateLists = List(
    "AIPHES_Data/LexSub/candidates/germanet_candidates",
    "AIPHES_Data/LexSub/candidates/germanet_candidates-hy",
    "AIPHES_Data/LexSub/candidates/germanet_candidates-hy-ho",
    "AIPHES_Data/LexSub/candidates/germeval_duden.de.txt")

  for (candidateList <- CandidateLists) {
    println(candidateList)
    val c = new SimpleCandidateFile(candidateList)
    println(evaluate(gold.items, c.get))
  }
}