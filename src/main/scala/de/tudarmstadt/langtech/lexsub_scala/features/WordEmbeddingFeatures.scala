package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordVectorFile

class WordEmbeddingSimilarity(val name: String, val embedding: WordVectorFile) extends NumericOptionalValueFeatureExtractor(name) {
  def extractOptValue(item: SubstitutionItem): Option[AnyVal] = embedding.cossim(item.targetLemma, item.substitution)
}