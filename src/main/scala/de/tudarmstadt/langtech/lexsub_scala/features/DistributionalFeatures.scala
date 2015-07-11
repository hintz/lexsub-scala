package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions


/** Utility wrapper for lookup-based DTs */
case class DTLookup(val dtName: String, val dt: WordSimilarityFile[String], 
    lookupFunction: Token => String,  // determines how to map token to lookup string
    equivalenceFunction: (Token, String) => Boolean = // determines equivalence between candidate and DT item
      (substitute, other) => substitute.lemma == other) {
  def similar(token: Token): Seq[(String, Double)] = (lookupFunction andThen dt.sim)(token)
  def similarity(token: Token, substitute: Token): Option[Double] = {
    val expansions = similar(token)
    val sim = expansions.collectFirst { case (other, score) if equivalenceFunction(substitute, other) => score }
    sim
  }
}

/** Looks up word similarity between target and substitute in a DT */
case class WordSimilarity(dt: DTLookup) extends LocalFeatureExtractor with NumericFeature  {
  val name = "Sim_" + dt.dtName
  def extract(item: SubstitutionItem): Seq[Feature] = dt.similarity(item.target, Token(item.substitution, item.target.pos, item.substitution))
}

/** Binary feature determining if substitution candidate is present in the DT features of target */
case class BinaryWordSimilarity(dt: DTLookup, k: Int) extends FeatureExtractor   {
  val name = "BinarySim_" + dt.dtName
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val expansions = dt.similar(item.lexSubInstance.head).take(k).map(_._1)
    item.candidates.map { c => 
      val candidate = Token(c, item.lexSubInstance.head.pos, c)
      val isContained = expansions.find { dtFeature => dt.equivalenceFunction(candidate, dtFeature) }.isDefined
      if (isContained) Seq(new Feature(name, true)) else Seq.empty[Feature]
    }
  }
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


case class SalientDTFeatures(dt: DTLookup) extends FeatureExtractor {
  
  val name = dt.dtName + "_ctxSaliency"
  
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val contextTokens = item.lexSubInstance.sentence.tokens.toSet
    def isInContext(dtFeature: String) = contextTokens.exists { token => dt.equivalenceFunction(token, dtFeature) }
    val salientFeatureSums = item.candidates.map { candidate => 
      val candidateToken = Token(candidate, item.lexSubInstance.head.pos, candidate)
      val subst = dt.similar(candidateToken)
      val salient = subst.collect { 
        case (dtFeature, weight) if isInContext(dtFeature) => 
          weight
      }
      salient.sum
    }
    val totalSum = salientFeatureSums.sum
    val results = salientFeatureSums.map(_ / totalSum)
    results map mkFeature
  }
  
  private def mkFeature(value: Double) = 
      if(!value.isNaN && value > 0) Seq(new Feature(name, value)) else Seq.empty[Feature]
}
