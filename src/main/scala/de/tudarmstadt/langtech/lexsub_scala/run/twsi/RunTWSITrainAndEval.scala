package de.tudarmstadt.langtech.lexsub_scala.run.twsi

import de.tudarmstadt.langtech.lexsub_scala.run.twsi.setup.CreateHoldOutSplit
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.ClearTKModel

object RunTWSITrainAndEval extends App {

  val model = new ClearTKModel("MaxEnt")
  val data = Settings.lexsubData.groupBy(_.getGold.gold.targetWord)
  val sampleData = CreateHoldOutSplit.sampleTargets.flatMap(data)

  val lexsub = model.train(
    sampleData,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
}