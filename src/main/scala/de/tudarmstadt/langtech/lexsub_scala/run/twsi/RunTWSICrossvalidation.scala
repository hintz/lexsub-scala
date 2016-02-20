package de.tudarmstadt.langtech.lexsub_scala.run.twsi

import de.tudarmstadt.langtech.lexsub_scala.training.ctk.DeprecatedTraining


object RunTWSICrossvalidation extends App {
 
  DeprecatedTraining.crossvalidate(
        Settings.lexsubData.take(100), 
        Settings.candidates.trainingList,
        Settings.candidates.systemList,
        Settings.features, 
        Settings.trainingDir, 
        Settings.instancesOutputFile,
        folds = 10,
        maxItems = 20)
}