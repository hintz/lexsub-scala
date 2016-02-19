package de.tudarmstadt.langtech.lexsub_scala.run.twsi

import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining
import de.tudarmstadt.langtech.lexsub_scala.run.twsi.setup.CreateHoldOutSplit

object RunTWSITrainAndEval extends App {

  val data = Settings.lexsubData.groupBy(_.getGold.gold.targetWord)
  val sampleData = CreateHoldOutSplit.sampleTargets.flatMap(data)

  CTKTraining.train(
    sampleData,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
}