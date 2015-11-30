package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.training.Training

object RunSemevalTrainAndEval extends App {
  
  
  val wordEmb = Settings.embeddings.levyContexts
  println(wordEmb.apply("door"))

  Training.train(
    Settings.semevalTest,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
    
  
}