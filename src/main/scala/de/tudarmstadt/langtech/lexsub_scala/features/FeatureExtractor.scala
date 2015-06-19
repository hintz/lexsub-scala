package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import org.cleartk.classifier.Instance
import scala.collection.JavaConversions._

trait FeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature]
}

abstract class NominalValueFeatureExtract(val featureName: String) extends FeatureExtractor {
  def extractValue(item: SubstitutionItem): String
  def extract(item: SubstitutionItem): Seq[Feature] = Seq(new Feature(featureName + "_" + extractValue(item), 1f))
}

abstract class NumericOptionalValueFeatureExtractor(val featureName: String) extends FeatureExtractor {
  def extractOptValue(item: SubstitutionItem): Option[AnyVal]
  def extract(item: SubstitutionItem): Seq[Feature] = extractOptValue(item).toSeq.map(new Feature(featureName, _))
}

abstract class NumericValueFeatureExtractor(override val featureName: String) extends NumericOptionalValueFeatureExtractor(featureName) {
  def extractValue(item: SubstitutionItem): AnyVal
  def extractOptValue(item: SubstitutionItem): Option[AnyVal] = Some(extractValue(item))
}

class FeatureAnnotator(extractors: FeatureExtractor*) {
  def annotate(item: SubstitutionItem) = extractors.flatMap(_.extract(item))

  def apply(item: SubstitutionItem): Instance[String] = {
    println("annotating " + item)
    val result = new Instance[String]
    val features = annotate(item)
    val outcome = if(item.isGood.get) "GOOD" else "BAD"
    result setOutcome outcome
    result addAll features
    result
  }
  
  def annotate(items: Iterable[SubstitutionItem]): Iterable[Seq[Feature]] = items.map(annotate)
  def apply(items: Iterable[SubstitutionItem]): Iterable[Instance[String]] = items.map(apply)
}
