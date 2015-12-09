package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.features.NGramLookup
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing

/** interface to simplify cross-language training */
trait LanguageData {
  
  def preprocessing: LexSubProcessing

  def candidates: CandidateList
  def ngrams: NGramLookup
  def conjunctions: Seq[String]

  def trainingData: Seq[LexSubInstance]
  //def testData: Seq[LexSubInstance]
  
  
  def trainingFolder: String
}