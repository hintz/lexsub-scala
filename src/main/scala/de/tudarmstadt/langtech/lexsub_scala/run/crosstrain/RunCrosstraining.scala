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
import org.cleartk.classifier.Instance

object RunCrosstraining extends App {
  
  val languages: List[LanguageData] = List(English, German, Italian)
  
  // featurize all data
  val features = languages map featurize
  val languagesWithTrainingData = languages.zip(features)
  
  // train all languages on their own data
  for((language, features) <- languagesWithTrainingData){
    println("Training " + language + "..")
    Training.trainAndPackage(features, language.trainingFolder)
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
    Training.trainAndPackage(combinedInstances, combinedFolder)
  }
  
  // train on all data
  println("Training on all languages combined..")
  val allLanguagesFolder = "trainingAllLanguages"
  val allFeatures = features.flatten
  Training.trainAndPackage(allFeatures, allLanguagesFolder)
  
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
      val selection = eval.lines.toList.filter(_.startsWith("precision ="))(0) // hacky grep for one line in the output
      println("> " + evaluationLanguge + " trained on " + trainLanguage + ": " + selection)
    }
    
    // evaluate on all other languages
    {
      val lexsub = mkLexsub(evaluationLanguge, evaluationLanguge.trainingFolderOther)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-others"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      val selection = eval.lines.toList.filter(_.startsWith("precision ="))(0) // hacky grep for one line in the output
      println("> " + evaluationLanguge + " trained on other languages:" + selection)
    }
    
    // evaluate on all languages
    {
      val lexsub = mkLexsub(evaluationLanguge, allLanguagesFolder)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-all"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      val selection = eval.lines.toList.filter(_.startsWith("precision ="))(0) // hacky grep for one line in the output
      println("> " + evaluationLanguge + " trained on all:"  + selection)
    }
  }
  
  println("Done.")
  
  def featurize(language: LanguageData) = {
    println("Featurizing " + language + "..")
    Training.featurize(language.trainingData, language.candidates, language.features)
  }

  def mkLexsub(targetLanguage: LanguageData, modelFolder: String): LexSub = 
    LexSubExpander(targetLanguage.candidates, targetLanguage.features, ClassifierScorer(modelFolder))

}