package de.tudarmstadt.langtech.lexsub_scala.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes

object RunGermevalOnGoldCandidates extends App {
  
 Training.crossvalidateOnGold(
      Settings.germevalTraining, 
      Settings.features, 
      Settings.trainingDir, 
      Settings.instancesOutputFile,
      folds = 10)
  
  /*
   * val data = Settings.germevalTraining
  printf("Will train on %d examples with gold candidates..\n", data.size)
  Training.trainOnGold(data, 
      Settings.features, 
      Settings.trainingDir)
  
  val lexsub = GoldCandidatesRanker(
      Settings.features, 
      ClassifierScorer(Settings.trainingDir))

  val outcomes = lexsub(data)
  
  // write results
  val results = Outcomes.collect(data, outcomes)
  SemEvalResultOutcomeWriter.save(results, Settings.instancesOutputFile)
  val oot =  Outcomes.evaluate(results, 10)
  val best = Outcomes.evaluate(results, 1)
  println("Evaluation: best=[%s] oot=[%s]".format(best, oot))

  println("Done.")
  * 
  */
}