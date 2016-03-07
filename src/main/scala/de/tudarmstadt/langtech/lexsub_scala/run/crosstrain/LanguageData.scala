package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.features.NGramLookup
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.lexsub_scala.features.WordVectorLookup
import de.tudarmstadt.langtech.lexsub_scala.features.Features

/** simplify resource interface to do cross-language training */
trait LanguageData {
  
  // a preprocessing pipeline for the language
  def preprocessing: LexSubProcessing

  // a single candidate list
  def candidates: CandidateList
  def goldCandidates: CandidateList
  
  // resources
  def ngrams: NGramLookup
  def conjunctions: Seq[String]
  def dtFirstOrder: DTLookup
  def dtSecondOrder: DTLookup
  def coocs: DTLookup
  def w2vEmbeddings: WordVectorLookup
  def wordEmbeddings: WordVectorLookup
  def contextEmbeddings: WordVectorLookup

  // each language can supply a test and trainset
  def trainingData: Seq[LexSubInstance]
  def testData: Seq[LexSubInstance]
  def allData: Seq[LexSubInstance] = trainingData ++ testData
  def testGoldfile: String
  
  // folder where this language's model will be stored
  def trainingFolder: String
  def trainingFolderOther: String = trainingFolder + "_other"
  def trainingOnlyFold(i: Int): String = trainingFolder + "/only-fold-" + i
  def trainingAllFold(i: Int): String = trainingFolder + "/all-fold-" + i
  
  // features. should be computed lazily
  def features: Features
  
  override def toString = getClass.getSimpleName.takeWhile(_.isLetterOrDigit)
}