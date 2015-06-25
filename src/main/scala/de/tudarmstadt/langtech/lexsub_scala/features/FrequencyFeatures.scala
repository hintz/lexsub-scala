package de.tudarmstadt.langtech.lexsub_scala.features

import com.googlecode.jweb1t.JWeb1TSearcher
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.utility
import java.util.logging.Logger
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions


trait NGramLookup {
  def apply(tokens: String*): Long
}

/** Web1T wrapper for NGramLookup */
case class Web1TLookup(web1t: JWeb1TSearcher) extends NGramLookup {
  // disable web1t logging, too much noise!
  try { Logger.getLogger("com.googlecode.jweb1t.JWeb1TSearcher").setLevel(java.util.logging.Level.OFF) }
  catch { case e: Exception => System.err.println("Web1T logging could not be disabled") }
  
  def apply(tokens: String*) = web1t.getFrequency(tokens :_*)
}

/** Sliding window for PairFreqRatio */
case class PairFreqRatios(nGrams: NGramLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends Features((for (l <- leftRange; r <- rightRange; if l + r < maxSize) yield PairFreqRatio(nGrams, l, r)): _*)

/** Sliding window for SetFreqRatio */
case class SetFreqRatios(nGrams: NGramLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends Features((for (l <- leftRange; r <- rightRange; if l + r < maxSize) yield SetFreqRatio(nGrams, l, r)): _*)

case class PairFreqRatio(nGrams: NGramLookup, left: Int, right: Int)
  extends SmartFeature[Option[(Vector[String], Long)]]
  with FeatureUtils {
  
  val name = "PairFreqRatio_" + left + "_" + right
  val slicer = utility.context[String](left, right) _

  def global(item: LexSubInstance): Option[(Vector[String], Long)] = {
    val sentence = item.sentence
    val originalTokens = sentence.tokens.map(_.word) // word forms, not lemmas
    val sliced = slicer(originalTokens, item.headIndex)

    if (sliced.exists(_.isEmpty)) // if slice doesn't fit, don't yield any feature
      return None
    val tokens = sliced.map(_.get).toVector
    val origFreq = nGrams(tokens: _*)
    Some(tokens, origFreq)
  }

  def extract(item: SubstitutionItem, global: Option[(Vector[String], Long)]): Seq[Feature] = {
    val substitute = item.substitution
    val result = global.map {
      case (tokens, origFreq) =>

        val replaced = tokens.updated(left, substitute)
        val replacedFreq = nGrams(replaced: _*)

        if (origFreq == 0|| replacedFreq == 0) // if original not found, don't yield any feature
          return None

        val ratio = replacedFreq.toDouble / (origFreq + replacedFreq)
        ratio
    }
    result
  }
}


case class SetFreqRatio(nGrams: NGramLookup, left: Int, right: Int) extends FeatureExtractor with FeatureUtils {
  val slicer = utility.context[String](left, right) _
	val name = "SetFreqRatio_" + left + "_" + right

  def extract(substitutions: Substitutions): Vector[Seq[Feature]] = {
    implicit val item = substitutions
    val sentence = item.lexSubInstance.sentence
    val originalTokens = sentence.tokens.map(_.word) // word forms, not lemmas
    val sliced = slicer(originalTokens, item.lexSubInstance.headIndex)
    
    // if slice doesn't fit, don't yield any feature
    if (sliced.exists(_.isEmpty)) return noFeatures
      
    val tokens = sliced.map(_.get).toVector
    
    val replacementFreqs = item.candidates.map { substitute => 
      val replaced = tokens.updated(left, substitute)
      val replacedFreq = nGrams(replaced: _*)
      replacedFreq.toDouble
    }
    // normalize with respect to all substitutions
    val normalized = utility.normalize(replacementFreqs)
    // yield only positive scores as feature
    val features = normalized.map(Some(_).filter(_ > 0))
    features.map(toFeatures)
  }
}


case class ConjunctionFreqRatio(nGrams: NGramLookup, conjunctions: Seq[String], left: Int, right: Int)
  extends SmartFeature[Option[(Vector[String], Long)]] {

  def global(item: LexSubInstance): Option[(Vector[String], Long)] = {
    val sentence = item.sentence
    val originalTokens = sentence.tokens.map(_.word) // word forms, not lemmas
    // slice adds words left and right to compensate for inserting (target, conjunction, substitute) in place of (target)
    val sliced = utility.context[String](left + 1, right + 1)(originalTokens, item.headIndex) 

    if (sliced.exists(_.isEmpty)) // if slice doesn't fit, don't yield any feature
      return None
    
    val tokens = sliced.map(_.get).toVector
    val origFreq = nGrams(tokens: _*)
    Some(tokens, origFreq)
  }

  def extract(item: SubstitutionItem, global: Option[(Vector[String], Long)]): Seq[Feature] = {
    val target = item.targetLemma
    val substitute = item.substitution
    
    val features = for (
        (tokens, origFreq) <- global.toSeq; 
        conj <- conjunctions;
        val replaced = tokens.take(left) ++ Seq(target, conj, substitute) ++ tokens.takeRight(right);
        val replacedFreq = nGrams(replaced: _*) if replacedFreq > 0)
    yield {
      if (origFreq < 10e-10) return Seq.empty // if original not found, don't yield any feature
      val ratio = replacedFreq.toDouble / origFreq
      val name = "ConjRatio_%d_%d_%s".format(left, right, conj)
      new Feature(name, ratio)
    }
    features
  }
}
