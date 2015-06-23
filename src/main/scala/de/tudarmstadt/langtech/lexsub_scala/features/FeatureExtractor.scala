package de.tudarmstadt.langtech.lexsub_scala.features

import org.cleartk.classifier.Feature
import org.cleartk.classifier.Instance
import scala.collection.JavaConversions._
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.utility.ReportingIterable._
import de.tudarmstadt.langtech.lexsub_scala.utility.BatchProcessing

abstract class FeatureExtractor {
  /** Yield a vector of feature vectors for each substitute */
  def extract(item: Substitutions): Vector[Seq[Feature]]
}

/** Features working only on SubstitutionItems */
abstract class LocalFeatureExtractor extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = item.asItems.map(extract)
  def extract(item: SubstitutionItem): Seq[Feature]
}

/** Features should implement this trait to avoid unnecessary computations */
abstract class SmartFeature[A] extends FeatureExtractor {
  def global(item: LexSubInstance): A
  def extract(item: SubstitutionItem, global: A): Seq[Feature]
  
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val g = global(item.lexSubInstance)
    item.asItems.map(extract(_, g))
  }
}

trait PureLocal extends SmartFeature[Null]{
  override def global(item: LexSubInstance) = null
}

/** Applies a collection of features */
class Features(features: FeatureExtractor*) extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    def combine(a: Vector[Seq[Feature]], b: Vector[Seq[Feature]]): Vector[Seq[Feature]] = {
      val comb: (Seq[Feature], Seq[Feature]) => Seq[Feature] = _ ++ _
      a.zip(b).map(comb.tupled)
    }
    features.map(_.extract(item)).reduce(combine)
  }
}



abstract class NominalOptionalValueFeatureExtract[T](val featureName: String) extends SmartFeature[T] {
  def extractOptValue(item: SubstitutionItem, global: T): Option[Any]
  def extract(item: SubstitutionItem, global: T): Seq[Feature] =
    extractOptValue(item, global).toSeq.map(v => new Feature(featureName, v))
}

abstract class NominalValueFeatureExtract[T](override val featureName: String) extends NominalOptionalValueFeatureExtract[T](featureName) {
  def extractValue(item: SubstitutionItem, global: T): Any
  override def extractOptValue(item: SubstitutionItem, global: T): Option[Any] = Some(extractValue(item, global))
}

abstract class NumericOptionalValueFeatureExtractor[T](val featureName: String) extends SmartFeature[T] {
  def extractOptValue(item: SubstitutionItem, global: T): Option[Double]
  def extract(item: SubstitutionItem, global: T): Seq[Feature] = extractOptValue(item, global).filter(!_.isNaN).toSeq.map(d => new Feature(featureName,d))
}

abstract class NumericValueFeatureExtractor[T](override val featureName: String) extends NumericOptionalValueFeatureExtractor[T](featureName) {
  def extractValue(item: SubstitutionItem): Double
  def extractOptValue(item: SubstitutionItem, global: T): Option[Double] = Some(extractValue(item))
}


/** Enhances Features with the option to create instances */
class FeatureAnnotator(features: FeatureExtractor*) 
  extends Features(features :_*)
  with BatchProcessing[Substitutions, Vector[Instance[String]]]
{

  private val mkFeature = (features: Seq[Feature], outcome: String) => {
    val result = new Instance[String]
    result setOutcome outcome
    result addAll features
    result
  }
  
  def apply(item: Substitutions): Vector[Instance[String]] = {
    val features = extract(item)
    val gold = item.asItems.map(_.isGood.get)
    val outcomes = gold.map(if(_) "GOOD" else "BAD")
    val instances = features.zip(outcomes).map(mkFeature.tupled)
    instances
  }
}

