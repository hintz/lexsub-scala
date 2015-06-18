package de.tudarmstadt.langtech.lexsub_scala.features

import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem


trait FeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature]
}

class FeatureAnnotator(extractors: FeatureExtractor*) {
  def annotate(item: SubstitutionItem) = extractors.flatMap(_.extract(item))
  def annotate(items: Iterable[SubstitutionItem]): Iterable[Seq[Feature]] = items.map(annotate)
}
