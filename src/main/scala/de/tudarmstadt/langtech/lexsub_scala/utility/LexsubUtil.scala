package de.tudarmstadt.langtech.lexsub_scala.utility

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.collections
import scala.util.Try
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.reader.LexItem
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.candidates.FixedCandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.LexSub

object LexsubUtil {
  
  def getCachefile(originalPath: String): String = "cache/%s.ser".format(originalPath.replaceAll("""[\/\.]+""","-"))
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file */
  def preprocessSemEval(folder: String, datafile: String, goldfile: String)
                       (implicit preprocessing: LexSubProcessing): Seq[LexSubInstance] = {
     val cachefile = getCachefile(folder + "/" + datafile)
     io.lazySerialized(cachefile){
      System.err.println("Cachefile %s does not exist, preprocessing SemEval data..".format(cachefile))
      val plainData = new SemEvalReader(folder, datafile, goldfile).items
      preprocessing.parseSemEval(plainData)
    }
  }
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file
   *  (default naming convention)
   */
  def preprocessSemEval(germevalFolder: String, filename: String)
                       (implicit preprocessing: LexSubProcessing): Seq[LexSubInstance] = 
    preprocessSemEval(germevalFolder, filename + ".xml", filename + ".gold")
    
  /** Pools gold candidates from instances */
  def poolGoldCandidatesFromInstances(data: Seq[LexSubInstance]): CandidateList = 
    poolGoldCandidates(data.map(_.getGold))
    
  /** Pools gold candidate file from semeval data */
  def poolGoldCandidates(data: Seq[SemEvalItem]): CandidateList = {
    val goldItems = data.map(_.gold)
    val byTarget = goldItems.groupBy(_.target)
    val result = byTarget.map { case (LexItem(word, pos), goldItems) => 
      val substitutes = goldItems.flatMap(_.substitutionWords).distinct
      val candidates = substitutes.map { s => Candidate(word, pos, s, Set("gold_candidate")) }
      (word, candidates)
    }
    new FixedCandidateList(result)
  }
  
  
  /** Creates folds for n-fold crossvalidation, by splitting on lexical items. Yields tuples of (heldOutData, restOfData)  */
  def createCVFolds(data: Seq[LexSubInstance], nFolds: Int): Seq[(Seq[LexSubInstance], Seq[LexSubInstance])] = {
    // first create CV splits by lexemes
    val lexicalItems = data.map(_.head)
    val cvSplit = collections.crossfold(lexicalItems, nFolds).toSeq
    // now map back to training data
    def extractSubset(lexicalItems: Seq[Token]): Seq[LexSubInstance] = data.filter(instance => lexicalItems.contains(instance.head))
    val result = for ((heldOutLex, restOfLex) <- cvSplit) yield (extractSubset(heldOutLex), extractSubset(restOfLex))
    result
  }
  
  /** Applies a set of LexSub subsystems (stemming from CV-training folds, and applies it to a list of eval folds */
  def mergeCVFolds(subsystems: Seq[LexSub], evalFolds: Seq[Seq[LexSubInstance]]): Iterable[Seq[(String, Double)]] = {
    val outcomes = subsystems.zip(evalFolds).flatMap { case (lexsub, evalData) => lexsub(evalData) }
    outcomes
  }
  
}