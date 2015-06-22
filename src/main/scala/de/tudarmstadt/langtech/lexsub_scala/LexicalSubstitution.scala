package de.tudarmstadt.langtech.lexsub_scala

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList

import org.cleartk.classifier.Classifier
import org.cleartk.classifier.Feature
import org.cleartk.classifier.jar.JarClassifierBuilder

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.utility.ReportingIterable._

/** Utility trait to run processing step on a collection of input */
trait BatchProcessing[In, Out] { 
	def apply(input: In): Out
  
  def report(i: Int, n: Int, passed: Double, remaining: Double){
    println("%d / %d items (%.2f%%) %.0fs passed, %.1fs remaining".format(i, n, i * 100f / n, passed, remaining))
  }
  
  def apply(input: Iterable[In]): Iterable[Out] = input.reporting(report, 5000).map(apply)
}

case class ClassifierScorer(val trainingDiretory: File) {
  
  import org.cleartk.classifier.mallet.MalletStringOutcomeClassifierBuilder
  import org.cleartk.classifier.Classifier
  import org.cleartk.classifier.Feature
    
  val GoodLabel = "GOOD"
  val classifier = JarClassifierBuilder
    .fromTrainingDirectory(trainingDiretory)
    .loadClassifierFromTrainingDirectory(trainingDiretory)
    .asInstanceOf[Classifier[String]]
  
  def apply(features: Seq[Feature]): Double = {
    val outcomes = classifier.score(features, 2)
    val goodScore = outcomes.collectFirst { case o if o.getOutcome == GoodLabel => o.getScore }
    goodScore.getOrElse(throw new IllegalStateException)
  }
}

case class LexSubExpander(
    val candidateList: CandidateList, 
    val featureAnnotator: FeatureAnnotator, 
    val scorer: ClassifierScorer,
    val maxItems: Int = 10) 
    extends BatchProcessing[LexSubInstance, Seq[(String, Double)]] {
  
  /** Expands and ranks */
  def apply(instance: LexSubInstance): Seq[(String, Double)] = {
    val candidates = candidateList(instance.head.lemma).filter(_ != instance.head.lemma)
    apply(instance, candidates)
  }
  
  /** Ranks predefined substitution candidates */
  def apply(instance: LexSubInstance, substitutions: Seq[String]): Seq[(String, Double)] = {
    val substItems = substitutions.map(new SubstitutionItem(instance, _))
    val scored = substitutions.zip(substItems).map { case (subst, substItem) =>
      val features = featureAnnotator.annotate(substItem)
      val score = scorer(features)
      (subst, score)
    }
    scored.sortBy(- _._2).take(maxItems)
  }
}
