package de.tudarmstadt.langtech.lexsub_scala.run.twsi.setup

import de.tudarmstadt.langtech.lexsub_scala.run.twsi.Settings
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalData
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader

object CreateCandidateList extends App {
  val gold = new SemEvalReader(Settings.twsiFolder, "twsi2.xml", "twsi2.gold")
  def extractLemmas(data: SemEvalData) = data.sentences.map(_.target).distinct
  val targets = extractLemmas(gold.data)
  io.write("resources/twsi/targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write("resources/twsi/targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
  
    // also create candidate list from training gold, for evaluation purposes
  val goldCandidates = gold.gold.items.flatMap { goldItem => 
    val target = goldItem.target
    val candidates = goldItem.substitutionWordsWithoutMultiwords
    candidates.map(c => Seq(target.word, target.pos, c, "gold_candidate").mkString("\t"))
  }.sorted.distinct
  io.write("resources/twsi/candidates/twsi_gold.tsv", goldCandidates.mkString("\n"))
}