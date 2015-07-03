package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.Settings
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalData
import de.tudarmstadt.langtech.scala_utilities.io

object CreateCandidateList extends App {
  
  // write all targets to targets.txt
  def extractLemmas(data: GermEvalData) = data.sentences.map(_.target).distinct
  val train = new GermEvalReader(Settings.germevalFolder, "train-dataset").data
  val test = new GermEvalReader(Settings.germevalFolder, "test-dataset").data
  val targets = extractLemmas(train) ++ extractLemmas(test)
  io.write("targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write("targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
  
  
}