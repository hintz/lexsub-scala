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
 
  // train all languages on their own data
  trainLanguage(Settings.English)
  trainLanguage(Settings.German)
  
  // create lexsub systems
  val englishFromEnglish = mkLexsub(Settings.English, Settings.English)
  val germanFromEnglish = mkLexsub(Settings.German, Settings.English)
  val englishFromGerman = mkLexsub(Settings.English, Settings.German)
  val germanFromGerman = mkLexsub(Settings.German, Settings.German)

  evaluate("English from English model", Settings.English.testData, englishFromEnglish)
  evaluate("German from English model", Settings.German.testData, germanFromEnglish)
  evaluate("German from German model", Settings.German.testData, germanFromGerman)
  evaluate("English from German model", Settings.English.testData, englishFromGerman)

  println("Done.")
  
  def trainLanguage(language: LanguageData){
    println("Training " + language + "..")
    Training.train(
      language.trainingData,
      language.candidates,
      language.features,
      language.trainingFolder)
  }
  
  def mkLexsub(targetLanguage: LanguageData, model: LanguageData): LexSub = 
    LexSubExpander(targetLanguage.candidates, targetLanguage.features, ClassifierScorer(model.trainingFolder))

  def evaluate(title: String, evalData: Seq[LexSubInstance], system: LexSub) {
    val outcomes = system(evalData)
    val results = Outcomes.collect(evalData, outcomes)
    val oot = Outcomes.evaluate(results, 10)
    val best = Outcomes.evaluate(results, 1)
    println(title + ": best=[%s] oot=[%s]".format(best, oot))
  }
}