package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.features.NGramLookup
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator

/** interface to simplify cross-language training */
trait LanguageData {
  
  // a preprocessing pipeline for the language
  def preprocessing: LexSubProcessing

  // a single candidate list
  def candidates: CandidateList
  
  // resources
  def ngrams: NGramLookup
  def conjunctions: Seq[String]
  //..

  // each language can supply a test and trainset
  def trainingData: Seq[LexSubInstance]
  def testData: Seq[LexSubInstance] = Seq.empty
  def allData: Seq[LexSubInstance] = trainingData ++ testData
  
  // folder where this language's model will be stored
  def trainingFolder: String
  
  // features. should be computed lazily
  def features: FeatureAnnotator
  
  override def toString = getClass.getSimpleName
}