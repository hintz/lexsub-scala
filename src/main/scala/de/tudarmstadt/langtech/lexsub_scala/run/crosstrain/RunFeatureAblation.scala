package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.English
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.German
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.Italian
import de.tudarmstadt.langtech.lexsub_scala.training.Featurizer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib.LambdaMart
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib.MAP
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator._
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib.NDCG
import de.tudarmstadt.langtech.lexsub_scala.features.NumLexSemRelations

object RunFeatureAblation extends App {
  
  val Array(ablationGroup) = args
  println("Evaluating ablation group " + ablationGroup)  
  
  val languages: List[LanguageData] = List(English, German, Italian)
  
  val cvFolds = 5
  val model: Model = RankLibModel(LambdaMart(MAP, 1000, 10))
  val trainingCandidateSelector: LanguageData => CandidateList = _.candidates
  val systemCandidateSelector: LanguageData => CandidateList = _.candidates
  
  val featureGroups: List[(String, LanguageData => Seq[FeatureExtractor])] = List(
     ("embeddingFeatures", { lang: LanguageData => Seq(
      // embedding n-grams
      WordEmbeddingDistanceVectorsSet(lang.w2vEmbeddings, 0 to 2, 0 to 2, 5),
      // Melamud's features
      SyntaxEmbeddingFeatures(lang.wordEmbeddings, lang.contextEmbeddings, Add, Mult, BalAdd, BalMult)
     ) }),
      
     ("semanticRelations", { lang: LanguageData => Seq(       
        NumLexSemRelations(lang.candidates),
        LexSemRelation(lang.candidates, simplifyLabels = true))}),
        
     ("frequencyFeatures", { lang: LanguageData => Seq(
      PairFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      SetFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(lang.ngrams, lang.conjunctions, 0, 0, false)
     ) }),
     
     ("distributionalFeatures", { lang: LanguageData => Seq(
               // to what extend the context characterizes the subst
      SalientDTFeatures(lang.dtFirstOrder),
      // similarity between target and subst
      /// WordSimilarity(dts.secondOrder),
      // top-k similar words
      AllThresholdedDTFeatures(
        dts = Seq(lang.dtSecondOrder),
        restrictToContext = Seq(false),
        Seq(5, 20, 50, 100, 200)),
      // top-k similar context-features, with and without restriction to sent context
      AllThresholdedDTFeatures(
        dts = Seq(lang.dtFirstOrder),
        restrictToContext = Seq(true, false),
        Seq(5, 20, 50, 100, 200)),
      // boolean feature if target/substitute are similar
      BinaryWordSimilarity(lang.dtSecondOrder, 100),
      
      // co-occurence features
      Cooc(lang.coocs)
     ) }),
     
     ("syntacticFeatures", { lang: LanguageData => Seq(
        // supply source language as constant feature
        ConstantFeature("SourceLanguage", lang.toString),
        
        // syntactic features
        PosContextWindow(0, 0),
        PosContextWindow(1, 1)
     )})
    )
    
    
  val ablationGroups = featureGroups.map(_._1)

  println("Performing feature ablation experiments with " + languages.mkString(", "))
  val threadpool = Executors.newFixedThreadPool(10)
  implicit val ec = ExecutionContext.fromExecutor(threadpool)
 
  //for(ablationGroup <- ablationGroups){
    //println("Evaluating ablation group " + ablationGroup)  
  
    // featurize all data no folds
    val featuresAllFutures = languages map { language => Future {
        val candidates = trainingCandidateSelector(language)
        println(s"Featurizing $language (all data) with candiates from $candidates..")
        io.lazySerialized("cache/" + language.toString + "-all-featurized-" + ablationGroup + "-ablated.ser" ){
            featurize(language, language.allData, candidates, ablationGroup)
          }
      }
    }
    
    
    println("Awaiting featurization for all languages..")
    val featuresAll = Await.result(Future.sequence(featuresAllFutures), Duration.Inf)
    
    // crossfold, and featurize all training sets for all folds
    println("Creating CV folds..")
    val crossfoldData = languages.map { lang => LexsubUtil.createCVFolds(lang.allData, cvFolds) }
    val cvHeldoutData = crossfoldData.map { cvData => cvData.map(_._1) }
    val cvTrainingData = crossfoldData.map { cvData => cvData.map(_._2) }
    // featurize crossfold data by looking it up in featuresAll
    val cvFeaturized = cvTrainingData.zip(featuresAll).map { case (trainingFolds, featurized) => 
      val lookupMap = featurized.map { case item@(Substitutions(instance, _), _)  => (instance, item) }.toMap
      val featurizedPerFold = trainingFolds.map { foldInstances => foldInstances.map(lookupMap) }
      featurizedPerFold
    }
  
    // some helper zippings
    val languagesWithAllTrainingData = languages.zip(featuresAll)
    val languagesWithTrainingFolds = languages.zip(cvFeaturized)
    val languagesWithHeldoutFolds = languages.zip(cvHeldoutData)
    val cvHeldoutLookup = languagesWithHeldoutFolds.toMap
  
    println("Begin training..")
    
    // train crossfold models for identity entries (languages on their own data), and all data (minus the current fold)
    val training = for((language, trainingFolds) <- languagesWithTrainingFolds; (fold, foldIdx) <- trainingFolds.zipWithIndex) yield {
  
      // train merged-with-all CV data
      val otherData = languagesWithAllTrainingData.flatMap { 
        case (otherLang, data) if otherLang != language => data 
        case (`language`, _) => List.empty
      } 
      val mergedData = otherData ++ fold
      val mergedFolder = modelDir(language, foldIdx, ablationGroup)
      println(s"Training $language fold $foldIdx (with all other data): " + mergedFolder)
      model.train(mergedData, mergedFolder)
  
    }
    
    // Wait for training to complete
    val allFutures = training
    val nJobs = allFutures.length
    var nCompleted = 0 // race-conditions aren't a tragedy here
    allFutures.foreach { f => f.onSuccess 
    { 
      case 0 => nCompleted += 1; println(s"Completed $nCompleted / $nJobs jobs")
      case _ => throw new RuntimeException("At least one job retured failure")
    }}
    println(s"Waiting for all training to complete ($nJobs jobs)..")
    val allTraining = Future.sequence(allFutures)
    val exitCodes = Await.result(allTraining, Duration.Inf)
    if(exitCodes.exists(_ != 0))
      throw new RuntimeException("Training yielded failure exit code: " + exitCodes)
    println("Completed all training.")
  
    
    /// --- Training complete. Start eval ---
    
    // evaluate all languages
    val results = for(evaluationLanguge <- languages) yield Future {
      println("Evaluating on " + evaluationLanguge + "..")
      val evalData = evaluationLanguge.testData
      val goldFile = evaluationLanguge.testGoldfile
      
      // cross-CV for all
      {
        val outFolder = "ablationResults/" + evaluationLanguge + "-on-all-cv-ablated-" + ablationGroup
        val heldoutFolds = cvHeldoutLookup(evaluationLanguge)
        
        val subsystems = heldoutFolds.indices.map { foldIdx => mkLexsub(evaluationLanguge, modelDir(evaluationLanguge, foldIdx, ablationGroup))}
        val outcomes = LexsubUtil.mergeCVFolds(subsystems, heldoutFolds)
        
        // important shadowing!
        val goldFile = evaluationLanguge.cvGoldfile
        
        val eval = SemEvalScorer.saveAndEvaluate(subsystems.head.toString, outcomes, Settings.scorerFolder, goldFile, outFolder)
        println("> " + evaluationLanguge + s" ABLATION -$ablationGroup all-$cvFolds-fold CV:" + SemEvalScorer.singleLine(eval))
      }
  
    }
    
    Await.result(Future.sequence(results), Duration.Inf)
    println("Done with " + ablationGroup)
    
  //}
  
  threadpool.shutdown
  RankLibWrapper.threadpool.shutdown
  println("Done.")
  
  def modelDir(language: LanguageData, foldIdx: Int, ablationGroup: String): String = language.trainingAllFold(foldIdx) + "-" + ablationGroup
  
  def mkFeatureset(language: LanguageData, ablationGroup: String): Features = {
    val features = featureGroups.collect { case (groupname, creator) if groupname != ablationGroup => creator(language) }.flatten
    new Features(features :_*)
  }
  
  def featurize(language: LanguageData, data: Seq[LexSubInstance], candidates: CandidateList, ablationGroup: String) = {
    val instances = Model.createTrainingData(data, candidates)
    val features = mkFeatureset(language, ablationGroup)
    Featurizer(features)(instances)
  }

  def mkLexsub(targetLanguage: LanguageData, modelFolder: String): LexSub = 
    LexSubExpander(systemCandidateSelector(targetLanguage), targetLanguage.features, model.getScorer(modelFolder))

}