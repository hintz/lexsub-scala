package de.tudarmstadt.langtech.lexsub_scala.utility

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.io
import scala.util.Try
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.reader.LexItem
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.candidates.FixedCandidateList

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

}