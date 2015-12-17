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
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer

object RunCrosstraining extends App {
  
  val languages = List(English, German, Italian)
  
  // train all languages on their own data
  // languages foreach trainLanguage
  
  // evaluate all languages
  for(evaluationLanguge <- languages){
    println("Evaluating on " + evaluationLanguge + "..")
    val evalData = evaluationLanguge.testData
    val goldFile = evaluationLanguge.testGoldfile
    for(trainLanguage <- languages){
      val lexsub = mkLexsub(evaluationLanguge, trainLanguage)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-" + trainLanguage
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      val selection = eval.lines.toList.filter(_.startsWith("precision ="))(1) // hacky grep for one line in the output
      println("> " + evaluationLanguge + " trained on " + trainLanguage + ": " + selection)
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

}