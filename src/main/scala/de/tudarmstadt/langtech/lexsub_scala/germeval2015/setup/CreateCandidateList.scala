package de.tudarmstadt.langtech.lexsub_scala.germeval2015.setup

import de.tudarmstadt.langtech.lexsub_scala.germeval2015.Settings
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalData
import de.tudarmstadt.langtech.scala_utilities.io

object CreateCandidateList extends App {
  
  // write all targets to targets.txt
  def extractLemmas(data: SemEvalData) = data.sentences.map(_.target).distinct
  val train = new SemEvalReader(Settings.germevalFolder, "train-dataset")
  val test = new SemEvalReader(Settings.germevalFolder, "test-dataset")
  val targets = extractLemmas(train.data) ++ extractLemmas(test.data)
  io.write("resources/germeval/targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write("resources/germeval/targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
  
  // also create candidate list from training gold, for evaluation purposes
  val goldCandidates = train.gold.items.flatMap { goldItem => 
    val target = goldItem.target
    val candidates = goldItem.substitutionWordsWithoutMultiwords
    candidates.map(c => Seq(target.word, target.pos, c, "gold_candidate").mkString("\t"))
  }.sorted.distinct
  io.write("resources/germeval/candidates/germeval_gold.tsv", goldCandidates.mkString("\n"))
}