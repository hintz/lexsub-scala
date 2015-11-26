package de.tudarmstadt.langtech.lexsub_scala.run.evalita2009

import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.run.twsi.setup.CreateHoldOutSplit
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter

object RunEvalitaTrainAndEval extends App {
  
  val trainingData = Settings.evalitaTest
  val evaluationData = Settings.evalitaTrial
  printf("Will train on %d examples and then lex-expand %d instances\n", trainingData.size, evaluationData.size)
  
  // do training
  Training.train(trainingData, 
      Settings.candidates.multiwordnet, 
      Settings.features, 
      Settings.trainingFolder)
  
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.multiwordnet,
      Settings.features, 
      ClassifierScorer(Settings.trainingFolder))

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