package de.tudarmstadt.langtech.lexsub_scala.twsi

import de.tudarmstadt.langtech.lexsub_scala.training.Training

object RunTWSITrainAndEval extends App {

  Training.train(
      Settings.lexsubData, 
      Settings.candidates.trainingList,
      Settings.features, 
      Settings.trainingDir)
}