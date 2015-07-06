package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.training.Training

object RunGermevalCrossvalidation extends App {
  Training.crossvalidate(
      Settings.germevalTraining, 
      Settings.candidates.masterlist, 
      Settings.features, 
      Settings.trainingDir, 
      Settings.instancesOutputFile,
      folds = 10)
}