package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import org.cleartk.classifier.Classifier
import org.cleartk.classifier.Feature
import org.cleartk.classifier.jar.JarClassifierBuilder
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.utility.BatchProcessing
import de.tudarmstadt.langtech.lexsub_scala.features.Features


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
    val features: Features,
    val scorer: ClassifierScorer,
    val maxItems: Int = 10) 
    extends BatchProcessing[LexSubInstance, Seq[(String, Double)]] {
  
  /** Expands and ranks */
  def apply(instance: LexSubInstance): Seq[(String, Double)] = {
    val candidates = candidateList(instance.head.lemma).filter(_ != instance.head.lemma)
    val substitutions = Substitutions(instance, candidates.toVector)
    apply(substitutions)
  }
  
  /** Ranks predefined substitution candidates */
  def apply(item: Substitutions): Seq[(String, Double)] = {
    val instances = features.extract(item)
    val scores = instances.map(scorer.apply)
    val scored = item.candidates.zip(scores)
    scored.sortBy(- _._2).take(maxItems)
  }
}
