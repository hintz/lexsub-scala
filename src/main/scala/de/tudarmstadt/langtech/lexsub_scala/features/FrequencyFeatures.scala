package de.tudarmstadt.langtech.lexsub_scala.features

import com.googlecode.jweb1t.JWeb1TSearcher
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.utility
import java.util.logging.Logger


case class Web1TFreqRatio(web1t: JWeb1TSearcher, left: Int, right: Int) 
extends NumericOptionalValueFeatureExtractor[Null]("FreqRatio_" + left + "_" + right) with PureLocal  {
   
  // disable web1t logging, too much noise!
  try { Logger.getLogger("com.googlecode.jweb1t.JWeb1TSearcher").setLevel(java.util.logging.Level.OFF)}
  catch { case e: Exception => System.err.println("Web1T logging could not be disabled") }

  val slicer = utility.context[String](left, right) _
   
   def extractOptValue(item: SubstitutionItem, global: Null): Option[Double] = {
     val sentence = item.lexSubInstance.sentence
     val originalTokens = sentence.tokens.map(_.word) // word forms, not lemmas
     val sliced = slicer(originalTokens, item.lexSubInstance.headIndex)
     
     if(sliced.exists(_.isEmpty)) // if slice doesn't fit, don't yield any feature
       return None
       
     val tokens = sliced.map(_.get).toVector
     val substitute = item.substitution
     val replaced = tokens.updated(left, substitute)
     
     val origFreq = web1t.getFrequency(tokens :_*)
     val replacedFreq = web1t.getFrequency(replaced :_*)
    
     
     if(origFreq < 10e-10) // if original not found, don't yield any feature
       return None
     
     val ratio = replacedFreq.toDouble / origFreq
     Some(ratio)
   }
 }

case class Web1TFreqRatios(web1t: JWeb1TSearcher, leftRange: Range, rightRange: Range, maxSize: Int) 
extends Features((for(l <- leftRange; r <- rightRange; if l + r < maxSize) yield Web1TFreqRatio(web1t, l, r)) :_*)


case class Web1TConjunctionRations(web1t: JWeb1TSearcher, conjunctions: Seq[String]){
    
}
