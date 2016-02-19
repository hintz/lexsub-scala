package de.tudarmstadt.langtech.lexsub_scala.run.twsi

import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining

object RunTWSICrossvalidation extends App {
 
  CTKTraining.crossvalidate(
        Settings.lexsubData.take(100), 
        Settings.candidates.trainingList,
        Settings.candidates.systemList,
        Settings.features, 
        Settings.trainingDir, 
        Settings.instancesOutputFile,
        folds = 10,
        maxItems = 20)
}