package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.distributional.DTFile
import de.tudarmstadt.langtech.lexsub_scala.types.Token


class DTLookup[Elem](val dtName: String, val dt: DTFile[Elem], lookupFunction: Token => String) {
  def similar(token: Token) = (lookupFunction andThen dt.sim)(token)
}

case class ThresholdedDTOverlap[E](dt: DTLookup[E], thresholds: Seq[Int], useLMIScores: Boolean) extends FeatureExtractor {
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
      new Feature(name, value)
    }
    features
  }
}
