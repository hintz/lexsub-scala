package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.setup

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalData
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.Settings

object CreateCandidateList extends App {
  val test = Settings.testData
  val trial = Settings.trialData
  val allGold = test.gold.items ++ trial.gold.items
  
  def extractLemmas(data: SemEvalData) = data.sentences.map(_.target).distinct
  val targets = extractLemmas(test.data) ++ extractLemmas(trial.data)
  io.write(Settings.resourcesFolder + "/targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write(Settings.resourcesFolder + "/targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
  
    // also create candidate list from training gold, for evaluation purposes
  val goldCandidates = allGold.flatMap { goldItem => 
    val target = goldItem.target
    val candidates = goldItem.substitutionWordsWithoutMultiwords
    candidates.map(c => Seq(target.word, target.pos, c, "gold_candidate").mkString("\t"))
  }.sorted.distinct
  io.write(Settings.resourcesFolder + "/candidates/semeval_gold.tsv", goldCandidates.mkString("\n"))
}