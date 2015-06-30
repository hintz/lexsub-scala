package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeWriter

object RunGermevalTrainingAndEval extends App {
  
  val trainingData = Settings.germevalTraining
  val evaluationData = Settings.germevalTraining
  
  printf("Will train on %d examples and then lex. expand %d instances\n", trainingData.size, evaluationData.size)
  
  // do training
  Training.train(trainingData, 
      Settings.candidates.duden, 
      Settings.features, 
      Settings.trainingDir)
  
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.duden,
      Settings.features, 
      ClassifierScorer(Settings.trainingDir))

  val outcomes = lexsub(evaluationData)
  
  // write results
  val results = Training.collectOutcomes(trainingData, outcomes)
  GermEvalResultOutcomeWriter.save(results, Settings.instancesOutputFile)

  println("Done.")
}