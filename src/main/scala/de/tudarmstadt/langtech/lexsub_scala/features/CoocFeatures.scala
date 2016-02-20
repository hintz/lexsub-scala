package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities._

/**
 * Given a co-occurence file *coocLookup* containing counts (or significance measures),
 * computes a score for each candidate item, based on the cooc-count overlap with the
 * context words of the sentence.
 */
case class Cooc(val coocLookup: DTLookup) extends FeatureExtractor with FeatureUtils {
  
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    implicit val substItem = item
    
    val contextTokens = item.lexSubInstance.sentence.tokens.map(_.word).toSet
    
    val (overlapCounts, overlapSum) = item.candidates.map { substitute =>
      val coocs = coocLookup.similar(Token(substitute, item.lexSubInstance.head.pos, substitute))
      val coocValues = coocs.toMap.filter(kv => compute.isFinite(kv._2)) //.mapValues(_.abs)
      val overlaps: Set[Double] = contextTokens.flatMap(coocValues.get)
      val overlapValue = overlaps.size
      (overlapValue, overlaps.sum)
    }.unzip
    
    if(overlapCounts.forall(_ == 0)) return noFeatures // if no overlaps are present, don't yield anything
    
    val counts = compute.absNormalize(overlapCounts.map(_.toDouble))
    val sums = compute.absNormalize(overlapSum)
    val sumsMax = compute.maxNormalize(overlapSum)
    
    val features = (counts, sums, sumsMax).zipped.map { 
      case (count, sum, sumMax) if count > 0 => Seq(NumericFeature("Cooc_count", count), NumericFeature("Cooc_sum", sum), NumericFeature("Cooc_sumM", sumMax))
      case _ => Seq.empty
    }
    features
  }
}