package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeReader

object PrettyPrintResults extends App {
  val input = "instances.out"
  val gold = Settings.germevalTraining
  val reader = new GermEvalResultOutcomeReader(gold)
  println(reader.prettyPrint(input))
  System.exit(0)
}