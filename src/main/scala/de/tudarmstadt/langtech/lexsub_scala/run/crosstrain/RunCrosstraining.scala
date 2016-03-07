package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.scala_utilities.io
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
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.DeprecatedTraining
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.ClearTKModel
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.Featurizer
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib._
import scala.concurrent.Future
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object RunCrosstraining extends App {
  
  val model: Model = RankLibModel(LambdaMart(GAP, 500, 16)) // new ClearTKModel("MaxEnt")
  
  val languages: List[LanguageData] = List(English, German, Italian)
  println("Performing crosstraining experiments with " + languages.mkString(", "))
  
  // featurize all data
  val features = languages map { language =>
    io.lazySerialized("cache/" + language.toString + "-training-featurized.ser" )(featurize(language))
  }
  val languagesWithTrainingData = languages.zip(features)
  
  // train all languages on their own data
  val training1 = for((language, featurized) <- languagesWithTrainingData) yield {
    println("Training " + language + "..")
    model.train(featurized, language.trainingFolder)
  }
  
  // train on other languages
  println("Training on combined other languages..")
  val training2 = for(i <- languages.indices) yield {
    val lang = languages(i)
    val otherData = languages.indices.diff(Seq(i)).map(languagesWithTrainingData)
    val (otherLangs, otherInstances) = otherData.unzip
    val combinedInstances = otherInstances.flatten
    val combinedFolder = lang.trainingFolderOther
    println("Training on combined set " + otherLangs.mkString("-") + " writing to " + combinedFolder)
    model.train(combinedInstances, combinedFolder)
  }
  
  // train on all data
  println("Training on all languages combined..")
  val allLanguagesFolder = "trainingAllLanguages"
  val allFeatures = features.flatten
  val training3 = model.train(allFeatures, allLanguagesFolder)
  
  
  println("Waiting for all training to complete..")
  implicit val ec = scala.concurrent.ExecutionContext.global
  val allTraining = Future.sequence(training1 ++ training2 ++ Seq(training3))
  allTraining.onSuccess { case results => println("Completed all training jobs with return values " + results) }
  Await.result(allTraining, Duration.Inf)
  
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
    val data = Model.createTrainingData(language.trainingData, language.goldCandidates)
    Featurizer(language.features)(data)
  }

  def mkLexsub(targetLanguage: LanguageData, modelFolder: String): LexSub = 
    LexSubExpander(targetLanguage.goldCandidates, targetLanguage.features, model.getScorer(modelFolder))

}