package de.tudarmstadt.langtech.lexsub_scala.features

import scala.collection.JavaConversions._
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.scala_utilities.processing.ReportingIterable._
import de.tudarmstadt.langtech.scala_utilities.processing.BatchProcessing

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

/** Applies a collection of features */
class Features(val extractors: FeatureExtractor*) extends FeatureExtractor {
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    def combine(a: Vector[Seq[Feature]], b: Vector[Seq[Feature]]): Vector[Seq[Feature]] = {
      //val comb: (Seq[Feature], Seq[Feature]) => Seq[Feature] = _ ++ _
      //a.zip(b).map(comb.tupled)
      (a, b).zipped.map(_ ++ _) // every now and then, this refuses to compile
    }
    val extracted = extractors.map(_.extract(item))
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