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

/** A scorer scores a given instance's features */
trait Scorer {
  def apply(features: Seq[Feature]): Double
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

case class GoldCandidatesRanker(
    val features: Features,
    val scorer: Scorer,
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


/** Enhances Features with the option to create training instances */
class FeatureAnnotator(features: FeatureExtractor*) 
  extends Features(features :_*)
  with BatchProcessing[Substitutions, Vector[Instance[String]]]
{

  private val mkInstance = (features: Seq[Feature], outcome: String) => {
    val result = new Instance[String]
    result setOutcome outcome
    result addAll features
    result
  }
  
  def apply(item: Substitutions): Vector[Instance[String]] = {
    val features = extract(item)
    val gold = item.asItems.map(_.isGood.get)
    val outcomes = gold.map(if(_) FeatureAnnotator.Good else FeatureAnnotator.Bad)
    val instances = features.zip(outcomes).map(mkInstance.tupled)
    instances
  }
}

object FeatureAnnotator {
  // some arbitrary labels for good and bad instances
  val Good = "GOOD"
  val Bad = "BAD"
}

/** A scorer extracting the value of a single named feature */
case class SingleFeatureScorer(/*val featureName: String, */val fallback: Double = Double.NaN) extends Scorer {
  def apply(features: Seq[Feature]): Double = {
    //val v = features.collectFirst { case f: Feature if f.getName == featureName => f.getValue.asInstanceOf[Double]}
    // v getOrElse fallback
    features match {
      case Seq(f) => f.getValue.asInstanceOf[Double]
      case Seq() => fallback
      case _ => throw new RuntimeException("requires at most one feature per item!")
    }
    
  }
}

/** A scorer based on a ClearTK classifier trained in trainingDirectory */
case class ClassifierScorer(val trainingDiretory: String) extends Scorer {
  import org.cleartk.classifier.mallet.MalletStringOutcomeClassifierBuilder
  import org.cleartk.classifier.Classifier
  import org.cleartk.classifier.Feature
    
  val classifier = JarClassifierBuilder
    .fromTrainingDirectory(new File(trainingDiretory))
    .loadClassifierFromTrainingDirectory(new File(trainingDiretory))
    .asInstanceOf[Classifier[String]]
  
  def apply(features: Seq[Feature]): Double = {
    val outcomes = classifier.score(features, 2)
    val goodScore = outcomes.collectFirst { case o if o.getOutcome == FeatureAnnotator.Good => o.getScore }
    goodScore.getOrElse(throw new IllegalStateException)
  }
}
