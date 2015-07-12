package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.Settings
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalData
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.collections
import de.tudarmstadt.langtech.lexsub_scala.germeval.GoldItem

object CreateCandidateList extends App {
  
  // write all targets to targets.txt
  def extractLemmas(data: GermEvalData) = data.sentences.map(_.target).distinct
  val train = new GermEvalReader(Settings.germevalFolder, "train-dataset")
  val test = new GermEvalReader(Settings.germevalFolder, "test-dataset")
  val targets = extractLemmas(train.data) ++ extractLemmas(test.data)
  io.write("resources/targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write("resources/targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
  
  // also create candidate list from training gold, for evaluation purposes
  val goldCandidates = train.gold.items.flatMap { goldItem => 
    val target = goldItem.target
    val candidates = goldItem.substitutionWordsWithoutMultiwords
    candidates.map(c => Seq(target.word, target.pos, c, "gold_candidate").mkString("\t"))
  }.sorted.distinct
  io.write("resources/candidates/germeval_gold.tsv", goldCandidates.mkString("\n"))
}