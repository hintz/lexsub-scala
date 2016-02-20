package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.training.ctk.DeprecatedTraining


object RunGermevalCrossvalidation extends App {
  DeprecatedTraining.crossvalidate(
      Settings.germevalTraining, 
      Settings.candidates.trainingList,
      Settings.candidates.systemList,
      Settings.features, 
      Settings.trainingDir, 
      Settings.instancesOutputFile,
      folds = 10,
      maxItems = 20)
}