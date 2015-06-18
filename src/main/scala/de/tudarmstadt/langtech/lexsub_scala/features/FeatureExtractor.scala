package de.tudarmstadt.langtech.lexsub_scala.features

import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem


trait FeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature]
}

abstract class NominalValueFeatureExtract(val featureName: String) extends FeatureExtractor {
  def extractValue(item: SubstitutionItem): String
  def extract(item: SubstitutionItem): Seq[Feature] = Seq(new Feature(featureName + "_" + extractValue(item), 1f))
}

abstract class NumericValueFeatureExtract(val featureName: String) extends FeatureExtractor {
  def extractValue(item: SubstitutionItem): AnyVal
  def extract(item: SubstitutionItem): Seq[Feature] = Seq(new Feature(featureName, extractValue(item)))
}


class FeatureAnnotator(extractors: FeatureExtractor*) {
  def annotate(item: SubstitutionItem) = extractors.flatMap(_.extract(item))
  def annotate(items: Iterable[SubstitutionItem]): Iterable[Seq[Feature]] = items.map(annotate)
}
