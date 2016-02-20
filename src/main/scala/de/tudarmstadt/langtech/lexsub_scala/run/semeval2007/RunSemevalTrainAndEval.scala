package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.ClearTKModel

object RunSemevalTrainAndEval extends App {
 
  val model = new ClearTKModel("MaxEnt")
  val trainingData = Settings.semevalTest
  val (evaluationData, evalGoldfile) = (Settings.semevalTrial, Settings.trialReader.gold.file)
  
  printf("Will train on %d examples and then lex-expand %d instances\n", trainingData.size, evaluationData.size)
    
  model.train(
    Settings.semevalTest,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
    
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.systemList,
      Settings.features, 
      model.getScorer(Settings.trainingDir))
      
  // run system
  val outcomes = lexsub(evaluationData)
  
  // eval and write results
  val results = Outcomes.collect(evaluationData, outcomes)
  val eval = SemEvalScorer.saveAndEvaluate(lexsub, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputSemeval")
  println(eval)
}