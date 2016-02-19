package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import org.cleartk.classifier.Classifier
import org.cleartk.classifier.Instance
import org.cleartk.classifier.Feature
import org.cleartk.classifier.jar.JarClassifierBuilder
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureExtractor
import de.tudarmstadt.langtech.scala_utilities.processing.BatchProcessing

/** A ranker, which given a feature vector of all substitutes, scores them */
trait Scorer {
  def apply(featureVector: Vector[Seq[Feature]]): Vector[Double]
}

/** A scorer scores a given instance's features */
trait PointwiseScorer extends Scorer {
  def apply(features: Seq[Feature]): Double
  def apply(featureVector: Vector[Seq[Feature]]): Vector[Double] = featureVector.map(apply)
}

/** Interface for LexSub systems */
abstract class LexSub extends BatchProcessing[LexSubInstance, Seq[(String, Double)]]{
  /** Expands and ranks */
  def apply(instance: LexSubInstance): Seq[(String, Double)]
  
  /** Ranks predefined substitution candidates */
  def apply(item: Substitutions): Seq[(String, Double)]
}

/** The central component of LexSub. Expands LexSubInstances with substitutions,
 *  according to a given CandidateList and a scorer
 */
case class LexSubExpander(
		val candidateList: CandidateList, 
		val features: Features,
		val scorer: Scorer,
		val maxItems: Int = Integer.MAX_VALUE) 
	  extends LexSub {
	
	/** Expands and ranks */
	def apply(instance: LexSubInstance): Seq[(String, Double)] = {
      // extract candidates from the candidateList and remove the target itself and all duplicates
			val candidates = candidateList(instance.head.lemma).filter(_ != instance.head.lemma).distinct
			val substitutions = Substitutions(instance, candidates.toVector)
			apply(substitutions)
	}
	
	/** Ranks predefined substitution candidates */
	def apply(item: Substitutions): Seq[(String, Double)] = {
			val instances = features.extract(item)
			val scores = scorer(instances)
			val scored = item.candidates.zip(scores)
			scored.sortBy(- _._2).take(maxItems)
	}
}

case class GoldCandidatesRanker(
    val features: Features,
    val scorer: PointwiseScorer,
    val maxItems: Int = Integer.MAX_VALUE) 
    extends LexSub {
  
  /** Ranks from gold */
  def apply(instance: LexSubInstance): Seq[(String, Double)] = {
      val candidates = instance.getGold.gold.substitutionWordsWithoutMultiwords
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

/** A scorer extracting the value of an only feature */
case class SingleFeatureScorer(val fallback: Double = Double.NaN) extends PointwiseScorer {
  def apply(features: Seq[Feature]): Double = {
    features match {
      case Seq(f) => f.getValue.asInstanceOf[Double]
      case Seq() => fallback
      case _ => throw new RuntimeException("requires at most one feature per item!")
    }
  }
}