package de.tudarmstadt.langtech.lexsub_scala.features

import com.googlecode.jweb1t.JWeb1TSearcher
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.utility


case class Web1TFreqRatio(web1t: JWeb1TSearcher, left: Int, right: Int) extends NumericOptionalValueFeatureExtractor("FreqRatio_" + left + "_" + right)  {
   val slicer = utility.context[String](left, right) _
  
   def extractOptValue(item: SubstitutionItem): Option[Double] = {
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