package de.tudarmstadt.langtech.lexsub_scala.utility

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.candidates.FixedCandidateList
import de.tudarmstadt.langtech.lexsub_scala.reader.GoldItem
import de.tudarmstadt.langtech.lexsub_scala.reader.LexItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalGold
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.scala_utilities.collections
import de.tudarmstadt.langtech.scala_utilities.io
import scala.concurrent.ExecutionContext

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
  
  
  def removeMWEfromGold(goldfile: String, targetFile: String) = {
    def accept(substitution: String) = !(substitution.contains(" ") || substitution.contains("-"))
    val items = new SemEvalGold(goldfile).items
    val filtered = items.map { case GoldItem(id, target, substitutions) =>
      GoldItem(id, target, substitutions.filter(s => accept(s._1)))
    }
    val lines = filtered.collect { case GoldItem(id, LexItem(word, pos), substitutions) if substitutions.nonEmpty =>
      s"$word.$pos $id :: " + substitutions.map { case (w, s) => w + " " + s}.mkString("", ";", ";")
    }
    val data = lines.mkString("\n")
    io.write(targetFile, data)
    println(s"Removed MWE from $goldfile, wrote to $targetFile")
  }
  
  
  /** Creates folds for n-fold crossvalidation, by splitting on lexical items. Yields tuples of (heldOutData, restOfData)  */
  def createCVFolds(data: Seq[LexSubInstance], nFolds: Int): Seq[(Seq[LexSubInstance], Seq[LexSubInstance])] = {
    // first create CV splits by lexemes
    val lexicalItems = data.map(_.head.lemma).distinct
    val cvSplit = collections.crossfold(lexicalItems, nFolds).toSeq
    // now map back to training data
    def extractSubset(lexicalItems: Seq[String]): Seq[LexSubInstance] = data.filter(instance => lexicalItems.contains(instance.head.lemma))
    val result = for ((heldOutLex, restOfLex) <- cvSplit) yield (extractSubset(heldOutLex), extractSubset(restOfLex))
    result
  }
  
  /** Applies a set of LexSub subsystems (stemming from CV-training folds, and applies it to a list of eval folds */
  def mergeCVFolds(subsystems: Seq[LexSub], evalFolds: Seq[Seq[LexSubInstance]])(implicit ec: ExecutionContext): Iterable[Outcome] = {
    val futures = subsystems.zip(evalFolds).map { case (lexsub, evalData) => Future { lexsub(evalData) } }
    val results = Await.result(Future.sequence(futures), Duration.Inf)
    Outcomes.collect(evalFolds.flatten, results.flatten)
  }
  
}