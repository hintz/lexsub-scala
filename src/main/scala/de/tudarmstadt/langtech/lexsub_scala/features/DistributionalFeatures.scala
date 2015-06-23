package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.types.Token


case class DTLookup(val dtName: String, val dt: WordSimilarityFile[String], 
    lookupFunction: Token => String,  // determines how to map token to lookup string
    equivalenceFunction: (String, String) => Boolean = // determines equivalence between candidate and similar item
      (substitute, other) => substitute == other) {
  def similar(token: Token) = (lookupFunction andThen dt.sim)(token)
  def similarity(token: Token, substitute: String) = {
    similar(token).collectFirst { case (other, score) if equivalenceFunction(substitute, other) => score }
  }
}

/** Looks up word similarity between target and substitute in a DTLookup */
case class WordSimilarity(dt: DTLookup) extends LocalFeatureExtractor with OptionalNumericFeature  {
  val name = "Sim_" + dt.dtName
  def extract(item: SubstitutionItem): Seq[Feature] = dt.similarity(item.target, item.substitution)
}

case class ThresholdedDTOverlap(dt: DTLookup, thresholds: Seq[Int], useLMIScores: Boolean) extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val substituteLemma = item.substitution
    val orig = dt.similar(item.target)
    val subst = dt.similar(Token(substituteLemma, item.target.pos, substituteLemma))
    val features = for(threshold <- thresholds) yield {
      val a = orig.take(threshold).toMap
      val b = subst.take(threshold).toMap
      val total = a.size + b.size
      val overlap = a.keySet.intersect(b.keySet)
      
      val name = dt.dtName + "_" + threshold + (if(useLMIScores) "_LMI" else "")
      val value = if(useLMIScores){
        val totalScore = a.values.sum + b.values.sum
        val intersectScore = overlap.map(k => a(k) + b(k)).sum
        intersectScore / totalScore
      }
      else {
        overlap.size.toDouble / total
      }
      if(!value.isNaN && value > 0) Some(new Feature(name, value)) else None
    }
    features.flatten
  }
}
