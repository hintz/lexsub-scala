package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.scala_utilities.io

object RunSemevalTrainAndEval extends App {
 
  val trainingData = Settings.semevalTest
  val evaluationData = Settings.semevalTrial
  
  printf("Will train on %d examples and then lex-expand %d instances\n", trainingData.size, evaluationData.size)
    
  Training.train(
    Settings.semevalTest,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
    
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.systemList,
      Settings.features, 
      ClassifierScorer(Settings.trainingDir))
      
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