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
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.{ English, German, Italian}

object RunCrosstraining extends App {
  
  val languages = List(English, German, Italian)
  
  // train all languages on their own data
  languages foreach trainLanguage
  
  // evaluate all languages
  for(evaluationLanguge <- languages){
    println("Evaluating on " + evaluationLanguge + "..")
    val testData = evaluationLanguge.testData
    for(trainLanguage <- languages){
      val lexsub = mkLexsub(evaluationLanguge, trainLanguage)
      evaluate("> " + evaluationLanguge + " trained on " + trainLanguage, testData, lexsub)
    }
  }
  
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