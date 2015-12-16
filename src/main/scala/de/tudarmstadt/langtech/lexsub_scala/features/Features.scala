package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types._

trait Feature {
  def name: String
  def value: AnyVal

  /** Converts any feature into a numeric feature */
  def asNumeric: NumericFeature = this match {
    case numeric: NumericFeature => numeric
    case NominalFeature(name, value) => NumericFeature(name + "_" + value, 1d)
    case BinaryFeature(name, value) =>  NumericFeature(name + "_" + value, 1d)
  }
}

object Feature {
  /** Shorthand for empty list of features */
  def nothing = Seq.empty[Feature]
}

case class NumericFeature(val name: String, val value: Double) extends Feature
case class NominalFeature(val name: String, val value: String) extends Feature
case class BinaryFeature(val name: String, val value: Boolean) extends Feature

/** Abstract feature extraction for lexical substitution */
abstract class FeatureExtractor {
  /** Yield a vector of multiple feature vectors for each substitute */
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

/** Features should generally implement this trait to avoid unnecessary computations:
 *  For a given LexSubInstance, a "global" value is computed once.
 *  This value is then passed to each SubstitutionItem to extract a per-item value */
abstract class SmartFeature[A] extends FeatureExtractor {
  def global(item: LexSubInstance): A
  def extract(item: SubstitutionItem, global: A): Seq[Feature]
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val g = global(item.lexSubInstance)
    item.asItems.map(extract(_, g))
  }
}

/** Some utility mixins for defining features */
trait FeatureUtils {
  val name: String

  implicit def noFeatures(implicit item: Substitutions) = Vector.fill(item.candidates.length)(Feature.nothing)

  implicit def asFeature(v: Double): Seq[Feature] = Seq(NumericFeature(name, v))
  def asFeature(v: Option[Double]): Seq[Feature] = v.toList.map(NumericFeature(name, _))

  implicit def asFeature(a: String): Seq[Feature] = Seq(NominalFeature(name, a))
  def asFeature(a: Option[String]): Seq[Feature] = a.toList.map(NominalFeature(name, _))
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

/** Aggregates an inner (real) feature with an aggregator function f */
class AggregatedFeature(val name: String, inner: FeatureExtractor, f: Seq[Double] => Double) extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    inner.extract(item).map { innerFeatures =>
      if(innerFeatures.isEmpty) Seq.empty
      else {
        val innerValues = innerFeatures.map(_.asNumeric.value)
        val result = f(innerValues)
        Seq(NumericFeature(name, result))
      }
     }
  }
}

/** Convenience class to load features from a pre-computed cache by supplying the instance object */
case class PrecomputedFeatureExtractor(val cache: LexSubInstance => Vector[Seq[Feature]]) extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = cache(item.lexSubInstance)
}