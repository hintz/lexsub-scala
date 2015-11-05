package de.tudarmstadt.langtech.lexsub_scala.twsi

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.twsi.setup.CreateHoldOutSplit

object RunTWSITrainAndEval extends App {

  val data = Settings.lexsubData.groupBy(_.getGold.gold.targetWord)
  val sampleData = CreateHoldOutSplit.sampleTargets.flatMap(data)

  Training.train(
    sampleData,
    Settings.candidates.trainingList,
    Settings.features,
    Settings.trainingDir)
}