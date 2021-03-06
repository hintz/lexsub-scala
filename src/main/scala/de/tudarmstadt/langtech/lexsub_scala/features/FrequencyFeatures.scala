package de.tudarmstadt.langtech.lexsub_scala.features


import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.scala_utilities.{collections, compute}
import java.util.logging.Logger
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil


trait NGramLookup {
  def apply(tokens: String*): Long
}

/** Web1T wrapper for NGramLookup */
case class Web1TLookup(web1tFolder: String, maxN: Int = 5) extends NGramLookup {
	import com.googlecode.jweb1t.JWeb1TSearcher
  import com.googlecode.jweb1t.JWeb1TIndexer
  import java.io.File
  
  // disable web1t logging, too much noise!
  try { Logger.getLogger("com.googlecode.jweb1t.JWeb1TSearcher").setLevel(java.util.logging.Level.OFF) }
  catch { case e: Exception => System.err.println("Web1T logging could not be disabled") }
  
  val cache = FileBackedCache(lookup, LexsubUtil.getCachefile(web1tFolder))
  
  val web1t: JWeb1TSearcher = {
    if(!new File(web1tFolder, "index-1gms").exists){
      System.err.printf("Web1T index in folder %s does not exist, creating..\n", web1tFolder)
      val idx = new JWeb1TIndexer(web1tFolder, maxN);
      idx.create
    }
    new JWeb1TSearcher(new File(web1tFolder), 1, maxN)
  }
  
  private def lookup(tokens: List[String]): java.lang.Long = web1t.getFrequency(tokens :_*)
  def apply(tokens: String*) = cache(tokens.toList)
  
  override def toString = "Web1TLookup(%s)".format(web1tFolder.replaceAll("""[\/\\]+""", "_"))
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
  
  implicit val name = "PairFreqRatio_" + left + "_" + right
  val slicer = collections.context[String](left, right) _

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
  val slicer = collections.context[String](left, right) _
	implicit val name = "SetFreqRatio_" + left + "_" + right

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
    val normalized = compute.normalize(replacementFreqs)
    // yield only positive scores as feature
    val features = normalized.map(Some(_).filter(_ > 0))
    features.map(asFeature)
  }
}

/**
 * @param conjunctions the set of conjunctions to create features for
 * @param left additional left tokens
 * @param right additional right tokens
 * @param includeConjunction if true, the language-dependant conjuction is added to the feature, if false only presence of conjuction is indicated
 */
case class ConjunctionFreqRatio(nGrams: NGramLookup, conjunctions: Seq[String], left: Int, right: Int, includeConjunction: Boolean = true)
  extends SmartFeature[Option[(Vector[String], Long)]] {

  def global(item: LexSubInstance): Option[(Vector[String], Long)] = {
    val sentence = item.sentence
    val originalTokens = sentence.tokens.map(_.word) // word forms, not lemmas
    // slice adds words left and right to compensate for inserting (target, conjunction, substitute) in place of (target)
    val sliced = collections.context[String](left + 1, right + 1)(originalTokens, item.headIndex) 

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
      val name = "ConjRatio_%d_%d%s".format(left, right, if(includeConjunction) "_" + conj else "")
      NumericFeature(name, ratio)
    }
    features
  }
}
