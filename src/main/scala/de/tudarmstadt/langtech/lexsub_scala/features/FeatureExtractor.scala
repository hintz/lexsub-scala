package de.tudarmstadt.langtech.lexsub_scala.features

import org.cleartk.classifier.Feature
import org.cleartk.classifier.Instance
import scala.collection.JavaConversions._
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.utility.ReportingIterable._
import de.tudarmstadt.langtech.lexsub_scala.utility.BatchProcessing

/** Abstract feature extraction for lexical substitution */
abstract class FeatureExtractor {
  /** Yield a vector of feature vectors for each substitute */
  def extract(item: Substitutions): Vector[Seq[Feature]]
}

/** Features working only on lexical substitution instances */
abstract class GlobalFeatureExtractor extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val feats = extract(item.lexSubInstance)
    Vector.fill(item.candidates.length)(feats)
  }
  def extract(item: LexSubInstance): Seq[Feature]
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

trait SimpleNominalFeature[A] {
  val name: String
  implicit def toFeatures(a: A): Seq[Feature] = Seq(new Feature(name, a))
}

trait OptionalNominalFeature[A] {
  val name: String
  implicit def toFeatures(a: Option[A]): Seq[Feature] = a.toList.map(new Feature(name, _))
}

trait SimpleNumericFeature {
  val name: String
  implicit def toFeatures(v: Double): Seq[Feature] = Seq(new Feature(name, v))
}

trait OptionalNumericFeature {
  val name: String
  implicit def toFeatures(v: Option[Double]): Seq[Feature] = v.toList.map(new Feature(name, _))
}


/** Applies a collection of features */
class Features(features: FeatureExtractor*) extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    def combine(a: Vector[Seq[Feature]], b: Vector[Seq[Feature]]): Vector[Seq[Feature]] = {
      //val comb: (Seq[Feature], Seq[Feature]) => Seq[Feature] = _ ++ _
      //a.zip(b).map(comb.tupled)
      (a, b).zipped.map(_ ++ _) // every now and then, this refuses to compile
    }
    val extracted = features.map(_.extract(item))
    val combined = extracted.reduce(combine)
    combined
  }
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