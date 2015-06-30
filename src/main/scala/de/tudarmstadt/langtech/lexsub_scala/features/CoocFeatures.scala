package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities._

case class Cooc(val coocLookup: DTLookup) extends FeatureExtractor with FeatureUtils {
  
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    implicit val substItem = item
    
    val contextTokens = item.lexSubInstance.sentence.tokens.map(_.word).toSet
    
    val (overlapCounts, overlapSum) = item.candidates.map { substitute =>
      val coocs = coocLookup.similar(Token(substitute, item.lexSubInstance.head.pos, substitute))
      val coocValues = coocs.toMap.mapValues(_.abs)
      val overlaps: Set[Double] = contextTokens.flatMap(coocValues.get)
      val overlapValue = overlaps.size
      (overlapValue, overlaps.sum)
    }.unzip
    
    if(overlapCounts.forall(_ == 0)) return noFeatures // if no overlaps are present, don't yield anything
    
    val counts = compute.normalize(overlapCounts.map(_.toDouble))
    val sums = compute.normalize(overlapSum)
    val features = (counts, sums).zipped.map { 
      case (count, sum) if count > 0 => Seq(new Feature("Cooc_count", count), new Feature("Cooc_sum", sum))
      case _ => Seq.empty
    }
    features
  }
}