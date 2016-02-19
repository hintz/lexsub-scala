package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.{ English, German, Italian}
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import org.cleartk.classifier.Instance
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining

object RunCrosstraining extends App {
  
  val languages: List[LanguageData] = List(English, German, Italian)
  println("Performing crosstraining experiments with " + languages.mkString(", "))
  
  // featurize all data
  val features = languages map featurize
  val languagesWithTrainingData = languages.zip(features)
  
  // train all languages on their own data
  for((language, features) <- languagesWithTrainingData){
    println("Training " + language + "..")
    CTKTraining.trainAndPackage(features, language.trainingFolder)
  }
  
  // train on other languages
  println("Training on combined other languages..")
  for(i <- languages.indices){
    val lang = languages(i)
    val otherData = languages.indices.diff(Seq(i)).map(languagesWithTrainingData)
    val (otherLangs, otherInstances) = otherData.unzip
    val combinedInstances = otherInstances.flatten
    val combinedFolder = lang.trainingFolderOther
    println("Training on combined set " + otherLangs.mkString("-") + " writing to " + combinedFolder)
    CTKTraining.trainAndPackage(combinedInstances, combinedFolder)
  }
  
  // train on all data
  println("Training on all languages combined..")
  val allLanguagesFolder = "trainingAllLanguages"
  val allFeatures = features.flatten
  CTKTraining.trainAndPackage(allFeatures, allLanguagesFolder)
  
  // evaluate all languages
  for(evaluationLanguge <- languages){
    println("Evaluating on " + evaluationLanguge + "..")
    val evalData = evaluationLanguge.testData
    val goldFile = evaluationLanguge.testGoldfile
    
    // evaluate on specific language
    for(trainLanguage <- languages){
      val lexsub = mkLexsub(evaluationLanguge, trainLanguage.trainingFolder)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-" + trainLanguage
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on " + trainLanguage + ": " + SemEvalScorer.singleLine(eval))
    }
    
    // evaluate on all other languages
    {
      val lexsub = mkLexsub(evaluationLanguge, evaluationLanguge.trainingFolderOther)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-others"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on other languages:" + SemEvalScorer.singleLine(eval))
    }
    
    // evaluate on all languages
    {
      val lexsub = mkLexsub(evaluationLanguge, allLanguagesFolder)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-all"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on all: "  + SemEvalScorer.singleLine(eval))
    }
  }
  
  println("Done.")
  
  def featurize(language: LanguageData) = {
    println("Featurizing " + language + "..")
    CTKTraining.featurize(language.trainingData, language.candidates, language.features)
  }

  def mkLexsub(targetLanguage: LanguageData, modelFolder: String): LexSub = 
    LexSubExpander(targetLanguage.candidates, targetLanguage.features, CTKScorer(modelFolder))

}