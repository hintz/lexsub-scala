package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile


case class DTLookup(val dtName: String, val dt: WordSimilarityFile[String], 
    lookupFunction: Token => String,  // determines how to map token to lookup string
    equivalenceFunction: (String, String) => Boolean = // determines equivalence between candidate and similar item
      (substitute, other) => substitute == other) {
  def similar(token: Token): Seq[(String, Double)] = (lookupFunction andThen dt.sim)(token)
  def similarity(token: Token, substitute: String) = {
    val expansions = similar(token)
    val sim = expansions.collectFirst { case (other, score) if equivalenceFunction(substitute, other) => score }
    sim
  }
}

/** Looks up word similarity between target and substitute in a DTLookup */
case class WordSimilarity(dt: DTLookup) extends LocalFeatureExtractor with NumericFeature  {
  val name = "Sim_" + dt.dtName
  def extract(item: SubstitutionItem): Seq[Feature] = dt.similarity(item.target, item.substitution)
}

case class ThresholdedDTOverlap(dt: DTLookup, thresholds: Seq[Int], useLMIScores: Boolean) extends LocalFeatureExtractor {
  
  private def mkFeature(name: String, value: Double) = 
      if(!value.isNaN && value > 0) Seq(new Feature(name, value)) else Seq.empty[Feature]
  
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val substituteLemma = item.substitution
    val orig = dt.similar(item.target)
    val subst = dt.similar(Token(substituteLemma, item.target.pos, substituteLemma))
    
    val features = for(threshold <- thresholds) yield {
      val a = orig.take(threshold).toMap
      val b = subst.take(threshold).toMap
      val overlap = a.keySet.intersect(b.keySet)
      val overlapSize = overlap.size.toDouble
      val name = dt.dtName + "_" + threshold
      
      // compute scores by different aggregations
      val byThreshold = mkFeature(name + "_thresh", overlapSize / threshold)
      val byLMI = if(useLMIScores) mkFeature(name + "_LMI", {
        val totalScore = a.values.sum + b.values.sum
        val intersectScore = overlap.map(k => a(k) + b(k)).sum
        intersectScore / totalScore
       }) else Seq.empty
       //val byContextSize = mkFeature(name + "_cont", overlapSize / math.min(a.size, b.size))
      
      byThreshold ++ byLMI //++ byContextSize
    }
    features.flatten
  }
}
