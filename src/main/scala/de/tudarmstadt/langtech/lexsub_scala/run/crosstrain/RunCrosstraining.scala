package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes

object RunCrosstraining extends App {

  // do training
  Training.train(Settings.English.data.trainingData,
    Settings.English.data.candidates,
    Settings.English.features,
    Settings.English.data.trainingFolder)

  // create Enlgish lexsub system
  val englishLexsub = LexSubExpander(
    Settings.English.data.candidates,
    Settings.English.features,
    ClassifierScorer(Settings.English.data.trainingFolder))

  // create German lexsub system
  val germanLexsub = LexSubExpander(
    Settings.German.data.candidates,
    Settings.German.features,
    ClassifierScorer(Settings.German.data.trainingFolder))
  
  val evalData = Settings.English.data.trainingData
  val outcomes = englishLexsub(evalData)
  val results = Outcomes.collect(evalData, outcomes)
  val oot =  Outcomes.evaluate(results, 10)
  val best = Outcomes.evaluate(results, 1)
  println("Evaluation: best=[%s] oot=[%s]".format(best, oot))

  println("Done.")
}