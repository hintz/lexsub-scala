package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeature
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator._
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeatures
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.ClearTKModel
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib._

/**
 * Runs an implementation of 
 * "A Simple Word Embedding Model for Lexical Substitution. (Oren Melamud, Omer Levy, and Ido Dagan. VSM Workshop 2015.)"
 */
object RunMelamud extends App {
  
  val candidates = Settings.candidates.gold
  
  // eval untrained single feature:
  for(metric <- List(Add, Mult, BalAdd, BalMult)){
    
      val singleFeature = new Features(SyntaxEmbeddingFeature(
      Settings.embeddings.levyWords, 
      Settings.embeddings.levyContexts, metric))
     val untrainedLexsub = LexSubExpander(
      candidates,
      singleFeature,
      SingleFeatureScorer())

      println("Evaluating pure Melamud: " + metric)
      //val (evaluationData, evalGoldfile) = (Settings.allData, Settings.cvGoldFile + ".nomwe")
      val (evaluationData, evalGoldfile) = (Settings.semevalTest, Settings.testReader.gold.file + ".nomwe")
      val outcomes = untrainedLexsub(evaluationData)
      val results = Outcomes.collect(evaluationData, outcomes)
      val eval = SemEvalScorer.saveAndEvaluate(untrainedLexsub.toString, results, Settings.scorerFolder, evalGoldfile, "outputMelamud")
      print("Untrained mode:\n" + eval)

  }
    
  // eval trained pipeline
  {
    
    val model: Model = RankLibModel(LambdaMart(NDCG(10), 1000, 10)) // new ClearTKModel("MaxEnt")
    val modelFolder = "trainingMelamud"
    
    val allFeatures = new Features(SyntaxEmbeddingFeatures(
      Settings.embeddings.levyWords, 
      Settings.embeddings.levyContexts,
      Add, Mult, BalAdd, BalMult))
      
    val trainingData = Settings.semevalTest
    val (evaluationData, evalGoldfile) = (Settings.semevalTest, Settings.testReader.gold.file + ".nomwe")
    
    println("Training supervised system..")
    val trainedLexsub = model.train(trainingData, candidates, allFeatures, modelFolder)
    
    val outcomes = trainedLexsub(evaluationData)
    val results = Outcomes.collect(evaluationData, outcomes)
    val eval = SemEvalScorer.saveAndEvaluate(trainedLexsub.toString, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputMelamud")
    print("Trained mode:\n" + eval)
  }
}