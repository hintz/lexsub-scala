package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.scala_utilities.compute
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions

/** Extracts lexical semantic features from candidate list */
case class LexSemRelation(candidates: CandidateList, simplifyLabels: Boolean = false) extends SmartFeature[Seq[Candidate]] { 

  /** Set of substrings used for label simplification */
  val SimplifiedRelations = List(Seq("synonym", "synset"), Seq("hypernym"), Seq("hyponym"))
  
  /** Simplifies the relation label */
  private def simplify(label: String): String = {
    
    val simplified = for(relations @ relname :: rest <- SimplifiedRelations; r <- relations if label contains r) yield relname
    simplified.headOption.getOrElse("other")
  }

  def global(item: LexSubInstance): Seq[Candidate] = candidates.get(item.head.lemma)
  def extract(item: SubstitutionItem, knownCandidates: Seq[Candidate]): Seq[Feature] = {
     val matching = knownCandidates.find(_.replacement == item.substitution)
     val relations = matching.map(_.relations).getOrElse(Set.empty)
     val simplified = if(simplifyLabels) relations.map(simplify) else relations
     val features = simplified map mkFeature
     features.toSeq
   }
  
  private def mkFeature(rel: String) = new Feature( "SemRel_" + rel, 1f)
}

/** Extracts te number of lexical semantic relations from candidate list */
case class NumLexSemRelations(candidates: CandidateList) extends FeatureExtractor with NumericFeature { 
  val name = "NumRelations"
  val inner = LexSemRelation(candidates)

  def extract(item: Substitutions): Vector[Seq[Feature]] = {
     val numRelations = inner.extract(item).map(_.length.toDouble)
     val result = compute.normalize(numRelations)
     result.map(toFeatures)
   }
}