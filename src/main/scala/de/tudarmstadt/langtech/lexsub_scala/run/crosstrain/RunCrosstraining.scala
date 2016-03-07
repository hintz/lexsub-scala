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
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList

object RunCrosstraining extends App {
  
  val languages: List[LanguageData] = List(English, German, Italian)
  
  val cvFolds = 10
  val model: Model = RankLibModel(LambdaMart(NDCG(10), 500, 16)) // new ClearTKModel("MaxEnt")
  val trainingCandidateSelector: LanguageData => CandidateList = _.goldCandidates

  println("Performing crosstraining experiments with " + languages.mkString(", "))
  
  // featurize all data no folds
  val featuresAll = languages map { language =>
    val candidates = trainingCandidateSelector(language)
    println(s"Featurizing $language (all data) with candiates from $candidates..")
    io.lazySerialized("cache/" + language.toString + "-all-featurized.ser" ){
      featurize(language, language.allData, candidates)
    }
  }
  
  // crossfold, and featurize all training sets for all folds
  val crossfoldData = languages.map { lang => LexsubUtil.createCVFolds(lang.allData, cvFolds) }
  val heldoutData = crossfoldData.map { cvData => cvData.map(_._1) }
  val cvTrainingData = crossfoldData.map { cvData => cvData.map(_._2) }
  val cvFeaturized = languages.zip(cvTrainingData).map { case (language, foldData) =>
    val candidates = trainingCandidateSelector(language)
    for ((foldTrainingData, foldIdx) <- foldData.zipWithIndex) yield {
          println(s"Featurizing $language fold $foldIdx (${foldTrainingData.length} instances)")
          io.lazySerialized("cache/" + language.toString + "-fold-" + foldIdx + "featurized.ser" ){
            featurize(language, foldTrainingData, candidates)
          }
    }
  }
  
  val languagesWithAllTrainingData = languages.zip(featuresAll)
  val languagesWithTrainingFolds = languages.zip(cvFeaturized)
  
  // train crossfold models for identity entries (languages on their own data), and all data (minus the current fold)
  val training1tmp = for((language, trainingFolds) <- languagesWithTrainingFolds; (fold, foldIdx) <- trainingFolds.zipWithIndex) yield {
    // train only-within-language CV data
    val modelFolder = language.trainingOnlyFold(foldIdx)
    println(s"Training $language fold $foldIdx (only self): " + modelFolder)
    val train1 = model.train(fold, modelFolder)
    
    // train merged-with-all CV data
    val otherData = languagesWithAllTrainingData.flatMap { 
      case (otherLang, data) if otherLang != language => data 
      case (`language`, _) => List.empty
    } 
    val mergedData = otherData ++ fold
    val mergedFolder = language.trainingAllFold(foldIdx)
    println(s"Training $language fold $foldIdx (with all other data): " + mergedFolder)
    val train2 = model.train(mergedData, mergedFolder)
    
    Seq(train1, train2)
  }
  val training1 = training1tmp.flatten

  // train on other languages
  println("Training on combined other languages..")
  val training2 = for(i <- languages.indices) yield {
    val lang = languages(i)
    val otherData = languages.indices.diff(Seq(i)).map(languagesWithAllTrainingData)
    val (otherLangs, otherInstances) = otherData.unzip
    val combinedInstances = otherInstances.flatten
    val combinedFolder = lang.trainingFolderOther
    println("Training on combined set " + otherLangs.mkString("-") + " writing to " + combinedFolder)
    model.train(combinedInstances, combinedFolder)
  }
  

  // train all languages on their full data
  val training3 = for((language, featurized) <- languagesWithAllTrainingData) yield {
    println("Training " + language + "..")
    model.train(featurized, language.trainingFolder)
  }
  
  /*
  // train on all data
  println("Training on all languages combined..")
  val allLanguagesFolder = "trainingAllLanguages"
  val allFeatures = features.flatten
  val training3 = model.train(allFeatures, allLanguagesFolder)
  */
  
  
  // Wait for training to complete
  println("Waiting for all training to complete..")
  implicit val ec = scala.concurrent.ExecutionContext.global
  val allTraining = Future.sequence(training1 ++ training2 ++ training3)
  val exitCodes = Await.result(allTraining, Duration.Inf)
  println("Completed all training jobs with return values " + exitCodes)
  if(exitCodes.exists(_ != 0))
    throw new RuntimeException("Training yielded failure exit code: " + exitCodes)
  
  /// --- Training complete. Start eval ---
  
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
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub.toString, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on " + trainLanguage + ": " + SemEvalScorer.singleLine(eval))
    }
    
    // evaluate on all other languages
    {
      val lexsub = mkLexsub(evaluationLanguge, evaluationLanguge.trainingFolderOther)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-others"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub.toString, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on other languages:" + SemEvalScorer.singleLine(eval))
    }
    
    /*
    // evaluate on all languages
    {
      val lexsub = mkLexsub(evaluationLanguge, allLanguagesFolder)
      val outcomes = lexsub(evalData)
      val outFolder = "crosstrainingResults/" + evaluationLanguge + "-on-all"
     
      val eval = SemEvalScorer.saveAndEvaluate(lexsub.toString, evalData, outcomes, Settings.scorerFolder, goldFile, outFolder)
      println("> " + evaluationLanguge + " trained on all: "  + SemEvalScorer.singleLine(eval))
    }
    */
  }
  
  println("Done.")
  
  def featurize(language: LanguageData, data: Seq[LexSubInstance], candidates: CandidateList) = {
    val instances = Model.createTrainingData(data, candidates)
    Featurizer(language.features)(instances)
  }

  def mkLexsub(targetLanguage: LanguageData, modelFolder: String): LexSub = 
    LexSubExpander(targetLanguage.goldCandidates, targetLanguage.features, model.getScorer(modelFolder))

}