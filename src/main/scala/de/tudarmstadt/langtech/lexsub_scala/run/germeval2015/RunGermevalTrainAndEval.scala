package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.ClearTKModel

object RunGermevalTrainingAndEval extends App {
  
  val model = new ClearTKModel("MaxEnt")
  val trainingData = Settings.germevalTraining
  val evaluationData = Settings.germevalTest
  
  printf("Will train on %d examples and then lex-expand %d instances\n", trainingData.size, evaluationData.size)
  
  // do training
  model.train(trainingData, 
      Settings.candidates.trainingList, 
      Settings.features, 
      Settings.trainingDir)
  
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.systemList,
      Settings.features, 
      model.getScorer(Settings.trainingDir))

  val outcomes = lexsub(evaluationData)
  
  // write results
  val results = Outcomes.collect(evaluationData, outcomes)
  SemEvalResultOutcomeWriter.save(results, Settings.instancesOutputFile)
  io.write(Settings.instancesOutputFile + ".system.txt", lexsub.toString)
  
  val oot =  Outcomes.evaluate(results, 10)
  val best = Outcomes.evaluate(results, 1)
  println("Evaluation: best=[%s] oot=[%s]".format(best, oot))

  println("Done.")
}