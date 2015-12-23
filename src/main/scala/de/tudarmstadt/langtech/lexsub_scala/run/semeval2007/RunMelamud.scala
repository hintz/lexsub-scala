package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeature
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator._
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeatures
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer

/**
 * Runs an implementation of 
 * "A Simple Word Embedding Model for Lexical Substitution. (Oren Melamud, Omer Levy, and Ido Dagan. VSM Workshop 2015.)"
 */
object RunMelamud extends App {
  
  val (evaluationData, evalGoldfile) = (Settings.semevalTrial, Settings.trialReader.gold.file)
  
  // define feature
  val allFeatures = new FeatureAnnotator(SyntaxEmbeddingFeatures(
    Settings.embeddings.levyWords, 
    Settings.embeddings.levyContexts,
    Add, Mult, BalAdd, BalMult))
  
  val singleFeature = new FeatureAnnotator(SyntaxEmbeddingFeature(
    Settings.embeddings.levyWords, 
    Settings.embeddings.levyContexts,
    BalMult))
  
  Training.train(evaluationData, Settings.candidates.wordnet, allFeatures, "trainingMelamud")
  
   // define lexsub system
   val trainedLexsub = LexSubExpander(
      Settings.candidates.wordnet,
      allFeatures,
      ClassifierScorer("trainingMelamud"))
      
      
    val untrainedLexsub = LexSubExpander(
      Settings.candidates.wordnet,
      singleFeature,
      SingleFeatureScorer())
      
    // eval untrained single feature:
    {
      val outcomes = untrainedLexsub(evaluationData)
      val results = Outcomes.collect(evaluationData, outcomes)
      val eval = SemEvalScorer.saveAndEvaluate(trainedLexsub, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputMelamud")
      print("Untrained mode:\n" + eval)
    }
      
    // eval trained pipeline
    {
      val outcomes = trainedLexsub(evaluationData)
      val results = Outcomes.collect(evaluationData, outcomes)
      val eval = SemEvalScorer.saveAndEvaluate(trainedLexsub, evaluationData, outcomes, Settings.scorerFolder, evalGoldfile, "outputMelamud")
      print("Trained mode:\n" + eval)
    }
}