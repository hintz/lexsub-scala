package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache
import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache


/** Utility wrapper for lookup-based DTs 
 *  The lookupFunction is used to look up DT entries based on a target token.
 *  The equivalenceFunction is used to determine equivalence between an (arbitrary) DT context feature and a given token
 *  Note: this allows to match e.g. only the lexical part of syntactic features, such as "-nsubj#throw" */
case class DTLookup(val dtName: String, val dt: WordSimilarityFile[String], 
    lookupFunction: Token => String,  // determines how to map token to lookup string
    equivalenceFunction: (Token, String) => Boolean = // determines equivalence between candidate and DT item
      (substitute, other) => substitute.lemma == other,
      cacheResults: Boolean = true) {
  
  private def fromFile(s: String): List[(String, Double)] =  dt.similar(s).toList
  private lazy val cache = FileBackedCache[String, List[(String, Double)]](fromFile, LexsubUtil.getCachefile(dt.dt_filename))
  private lazy val lookup = if(cacheResults) cache.apply _ else fromFile _

  def similar(token: Token): Seq[(String, Double)] = (lookupFunction andThen lookup)(token)
  def similarity(token: Token, substitute: Token): Option[Double] = {
    val expansions = similar(token)
    val sim = expansions.collectFirst { case (other, score) if equivalenceFunction(substitute, other) => score }
    sim
  }
  
  override def toString = "DT(%s)".format(dtName)
}

/** Looks up word similarity between target and substitute in a DT.
 *  Note: This requires the DT to contain second order features, aka similar words  */
case class WordSimilarity(dt: DTLookup) extends LocalFeatureExtractor with FeatureUtils {
  implicit val name = "Sim_" + dt.dtName
  def extract(item: SubstitutionItem): Seq[Feature] = dt.similarity(item.target, Token(item.substitution, item.target.pos, item.substitution))
}

/** Similar to WordSimilarity, but only a binary feature:
 *  Determins if substitution candidate is present in the top k DT features of target */
case class BinaryWordSimilarity(dt: DTLookup, k: Int) extends FeatureExtractor   {
  val name = "BinarySim_" + dt.dtName + "_" + k
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val expansions = dt.similar(item.lexSubInstance.head).take(k).map(_._1)
    item.candidates.map { c => 
      val candidate = Token(c, item.lexSubInstance.head.pos, c)
      val isContained = expansions.find { dtFeature => dt.equivalenceFunction(candidate, dtFeature) }.isDefined
      if (isContained) Seq(NumericFeature(name, 1d)) else Feature.nothing
    }
  }
}



/** Feature measuring the overlap between a target word and a substitute given a DT with arbitrary context features.
 *  @param useContextFilter: restricts the featureoverlap 
 */
case class ThresholdedDTOverlap(dt: DTLookup, thresholds: Seq[Int], useLMIScores: Boolean, useContextFilter: Boolean) 
extends SmartFeature[ThresholdedDTCache] {  
  
  def global(item: LexSubInstance): ThresholdedDTCache = {
    val contextTokens = item.sentence.tokens.toSet
    def isInContext(dtFeature: String) = contextTokens.exists { token => dt.equivalenceFunction(token, dtFeature) }
    val similar = dt.similar(item.head)
    ThresholdedDTCache(similar, isInContext)
   }
  
  def extract(item: SubstitutionItem, global: ThresholdedDTCache): Seq[Feature] = {
    val ThresholdedDTCache(orig, isInContext) = global
    val contextFilter: ((String, Double)) => Boolean = 
      if(useContextFilter) x => isInContext(x._1)
      else _ => true
    
    val substituteLemma = item.substitution
    val subst = dt.similar(Token(substituteLemma, item.target.pos, substituteLemma))
    
    val features = for(threshold <- thresholds) yield {
      val a = orig.take(threshold).filter(contextFilter).toMap
      val b = subst.take(threshold).filter(contextFilter).toMap
      val overlap = a.keySet.intersect(b.keySet)
      val overlapSize = overlap.size.toDouble
      val name = dt.dtName + "_" + threshold + (if(useContextFilter) "_ctx" else "")
      
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

  private def mkFeature(name: String, value: Double) =
    if (!value.isNaN && value > 0) Seq(NumericFeature(name, value)) else Feature.nothing
}

/** Helper class for ThresholdedDTOverlap */ 
case class ThresholdedDTCache(val origSimilar: Seq[(String, Double)], val contextFilter: String => Boolean)

/** Similarity based on "salient" DT features.
 *  A DT-feature is salient if it occurs in the sentence context (based on the equivalence function in the given DTLookup)
 *  This feature simply aggregates the weights of all salient features and normalized over all candidates
 * 
 *  Note: 
 *  This feature is near-equivalent to the "Cooc" feature, except that a custom equivalence function is specified via the DTLookup
 */
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
      if(!value.isNaN && value > 0) Seq(NumericFeature(name, value)) else Seq.empty[Feature]
}

/** Aggregates multiple ThresholdedDTOverlap features based on the given parameters */
case class AllThresholdedDTFeatures(dts: Seq[DTLookup], restrictToContext: Seq[Boolean], thresholds: Seq[Int])
  extends Features((for (dt <- dts; useLMI <- Seq(true, false); useContext <- restrictToContext)
    yield ThresholdedDTOverlap(dt, thresholds, useLMI, useContext)): _*)
