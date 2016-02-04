package de.tudarmstadt.langtech.lexsub_scala.run.evalita2009

import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.run.twsi.setup.CreateHoldOutSplit
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer

object RunEvalitaTrainAndEval extends App {
  
  val trainingData = Settings.evalitaTest
  val evaluationData = Settings.evalitaTrial
  printf("Will train on %d examples and then lex-expand %d instances\n", trainingData.size, evaluationData.size)
  
  // do training
  Training.train(trainingData, 
      Settings.candidates.systemList, 
      Settings.features, 
      Settings.trainingFolder)
  
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.systemList,
      Settings.features, 
      ClassifierScorer(Settings.trainingFolder))

  // process evaluation items
  val outcomes = lexsub(evaluationData)
  
  // my evaluation:
  val results = Outcomes.collect(evaluationData, outcomes)
  val oot =  Outcomes.evaluate(results, 10)
  val best = Outcomes.evaluate(results, 1)
  
  // perl script evaluation
  val eval = SemEvalScorer.saveAndEvaluate(lexsub, evaluationData, outcomes, Settings.scorerFolder, Settings.trialGoldfile, Settings.outputFolder)
  
  println("Evalita: best=[%s] oot=[%s]".format(best, oot)) // my format
  println("> " + SemEvalScorer.singleLine(eval)) // semeval scorer main metric
  println(eval) // full semeval scorer output
}