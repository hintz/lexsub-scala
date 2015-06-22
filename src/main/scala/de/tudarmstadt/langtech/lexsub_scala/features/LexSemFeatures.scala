package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature

/** Extracts lexical semantic features from candidate list */
case class LexSemRelation(candidates: CandidateList) extends FeatureExtractor { 

  private def mkFeature(rel: String) = new Feature( "SemRel_" + rel, 1f)

  def extract(item: SubstitutionItem): Seq[Feature] = {
     val knownCandidates = candidates.get(item.targetLemma)
     val matching = knownCandidates.find(_.replacement == item.substitution)
     val relations = matching.map(_.relations).getOrElse(Set.empty)
     val features = relations map mkFeature
     features.toSeq
   }
}