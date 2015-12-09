package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.Scorer

object RunCrosstraining extends App {
  
  println("Training on German..")
  Training.train(Settings.German.data.trainingData,
    Settings.German.data.candidates,
    Settings.German.features,
    Settings.German.data.trainingFolder)

  println("Training on English..")
  Training.train(Settings.English.data.trainingData,
    Settings.English.data.candidates,
    Settings.English.features,
    Settings.English.data.trainingFolder)

  // create lexsub systems
  val englishFromEnglish = LexSubExpander(
    Settings.English.data.candidates,
    Settings.English.features,
    ClassifierScorer(Settings.English.data.trainingFolder))

  val germanFromEnglish = LexSubExpander(
    Settings.German.data.candidates,
    Settings.German.features,
    ClassifierScorer(Settings.English.data.trainingFolder))

  val englishFromGerman = LexSubExpander(
    Settings.English.data.candidates,
    Settings.English.features,
    ClassifierScorer(Settings.German.data.trainingFolder))

  val germanFromGerman = LexSubExpander(
    Settings.German.data.candidates,
    Settings.German.features,
    ClassifierScorer(Settings.German.data.trainingFolder))

  evaluate("English from English model", Settings.English.data.trainingData, englishFromEnglish)
  evaluate("German from English model", Settings.German.data.trainingData, germanFromEnglish)
  evaluate("German from German model", Settings.German.data.trainingData, germanFromGerman)
  evaluate("English from German model", Settings.English.data.trainingData, englishFromGerman)

  println("Done.")

  def evaluate(title: String, evalData: Seq[LexSubInstance], system: LexSub): String = {
    val outcomes = system(evalData)
    val results = Outcomes.collect(evalData, outcomes)
    val oot = Outcomes.evaluate(results, 10)
    val best = Outcomes.evaluate(results, 1)
    title + ": best=[%s] oot=[%s]".format(best, oot)
  }
}