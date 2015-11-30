package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.types.Token

object RunSemevalTrainAndEval extends App {
  
  val dt = Settings.dts.secondOrder
  dt.similar(Token("job", "n", "job")) foreach println
  
  
  //val wordEmb = Settings.embeddings.levyContexts
  //println(wordEmb.apply("door"))

  Training.train(
    Settings.semevalTest,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
    
  
}