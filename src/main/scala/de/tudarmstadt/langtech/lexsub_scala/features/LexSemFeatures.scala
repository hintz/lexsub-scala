package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions

/** Extracts lexical semantic features from candidate list */
case class LexSemRelation(candidates: CandidateList) extends SmartFeature[Seq[Candidate]] { 

  private def mkFeature(rel: String) = new Feature( "SemRel_" + rel, 1f)

  def global(item: LexSubInstance): Seq[Candidate] = candidates.get(item.head.lemma)
  def extract(item: SubstitutionItem, knownCandidates: Seq[Candidate]): Seq[Feature] = {
     val matching = knownCandidates.find(_.replacement == item.substitution)
     val relations = matching.map(_.relations).getOrElse(Set.empty)
     val features = relations map mkFeature
     features.toSeq
   }
}

/** Extracts te number of lexical semantic relations from candidate list */
case class NumLexSemRelations(candidates: CandidateList) extends FeatureExtractor with NumericFeature { 
  val name = "NumRelations"
  val inner = LexSemRelation(candidates)

  def extract(item: Substitutions): Vector[Seq[Feature]] = {
     val numRelations = inner.extract(item).map(_.length.toDouble)
     numRelations.map(toFeatures)
   }
}