package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeReader

object PrettyPrintResults extends App {
  val input = "instances.out"
  val gold = Settings.germevalTraining
  val reader = new SemEvalResultOutcomeReader(gold)
  println(reader.prettyPrint(input))
  System.exit(0)
}