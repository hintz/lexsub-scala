package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import org.cleartk.classifier.Instance
import scala.collection.JavaConversions._
import de.tudarmstadt.langtech.lexsub_scala.utility.ReportingIterable._

trait FeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature]
}

class FeatureExtractorCollection(features: FeatureExtractor*) extends FeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = features.flatMap(_.extract(item))
}

abstract class NominalOptionalValueFeatureExtract(val featureName: String) extends FeatureExtractor {
  def extractOptValue(item: SubstitutionItem): Option[Any]
  def extract(item: SubstitutionItem): Seq[Feature] = extractOptValue(item).toSeq.map(v => new Feature(featureName, v)) // new Feature(featureName + "_" + v, 1f)
}

abstract class NominalValueFeatureExtract(override val featureName: String) extends NominalOptionalValueFeatureExtract(featureName) {
  def extractValue(item: SubstitutionItem): Any
  override def extractOptValue(item: SubstitutionItem): Option[Any] = Some(extractValue(item))
}

abstract class NumericOptionalValueFeatureExtractor(val featureName: String) extends FeatureExtractor {
  def extractOptValue(item: SubstitutionItem): Option[Double]
  def extract(item: SubstitutionItem): Seq[Feature] = extractOptValue(item).filter(!_.isNaN).toSeq.map(d => new Feature(featureName,d))
}

abstract class NumericValueFeatureExtractor(override val featureName: String) extends NumericOptionalValueFeatureExtractor(featureName) {
  def extractValue(item: SubstitutionItem): Double
  def extractOptValue(item: SubstitutionItem): Option[Double] = Some(extractValue(item))
}

class FeatureAnnotator(extractors: FeatureExtractor*) {
  def annotate(item: SubstitutionItem) = extractors.flatMap(_.extract(item))

  def apply(item: SubstitutionItem): Instance[String] = {
    val result = new Instance[String]
    val features = annotate(item)
    val outcome = if(item.isGood.get) "GOOD" else "BAD"
    result setOutcome outcome
    result addAll features
    result
  }
  
  def annotate(items: Iterable[SubstitutionItem]): Iterable[Seq[Feature]] = items.map(annotate)
  def apply(items: Iterable[SubstitutionItem]): Iterable[Instance[String]] = items.reporting(report, 5000).map(apply)
    
    
  def report(i: Int, n: Int, passed: Double, remaining: Double){
    println("%d / %d items (%.2f%%) %.0fs passed, %.0fs remaining".format(i, n, i * 100f / n, passed, remaining))
  }
}
