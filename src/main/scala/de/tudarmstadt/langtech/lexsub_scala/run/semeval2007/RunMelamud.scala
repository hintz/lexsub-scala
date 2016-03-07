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

/**
 * Runs an implementation of 
 * "A Simple Word Embedding Model for Lexical Substitution. (Oren Melamud, Omer Levy, and Ido Dagan. VSM Workshop 2015.)"
 */
object RunMelamud extends App {
  
  val model = new ClearTKModel("MaxEnt")
  val modelFolder = "trainingMelamud"
  val trainingData = Settings.semevalTest
  val (evaluationData, evalGoldfile) = (Settings.semevalTrial, Settings.trialReader.gold.file)
  
  // define feature
  val allFeatures = new Features(SyntaxEmbeddingFeatures(
    Settings.embeddings.levyWords, 
    Settings.embeddings.levyContexts,
    Add, Mult, BalAdd, BalMult))
  
  val singleFeature = new Features(SyntaxEmbeddingFeature(
    Settings.embeddings.levyWords, 
    Settings.embeddings.levyContexts,
    BalMult))
  
  val trainedLexsub = model.train(trainingData, Settings.candidates.wordnet, allFeatures, modelFolder)

  val untrainedLexsub = LexSubExpander(
    Settings.candidates.wordnet,
    singleFeature,
    SingleFeatureScorer())
    
  // eval untrained single feature:
  {
    val outcomes = untrainedLexsub(evaluationData)
    val results = Outcomes.collect(evaluationData, outcomes)
    val eval = SemEvalScorer.saveAndEvaluate(trainedLexsub.toString, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputMelamud")
    print("Untrained mode:\n" + eval)
  }
    
  // eval trained pipeline
  {
    val outcomes = trainedLexsub(evaluationData)
    val results = Outcomes.collect(evaluationData, outcomes)
    val eval = SemEvalScorer.saveAndEvaluate(trainedLexsub.toString, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputMelamud")
    print("Trained mode:\n" + eval)
  }
}