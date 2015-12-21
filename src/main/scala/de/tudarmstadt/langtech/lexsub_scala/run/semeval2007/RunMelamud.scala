package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeature
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator._
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.training.Training

/**
 * Runs an implementation of 
 * "A Simple Word Embedding Model for Lexical Substitution. (Oren Melamud, Omer Levy, and Ido Dagan. VSM Workshop 2015.)"
 */
object RunMelamud extends App {
  
  val evaluationData = Settings.semevalTrial
  
  // define feature
  val melamudsFeature = SyntaxEmbeddingFeature(
    Settings.embeddings.levyWords, 
    Settings.embeddings.levyContexts,
    BalAdd)
    
  val features = new FeatureAnnotator(melamudsFeature)
  
  // this is not needed, just for debugging
  //Training.train(evaluationData, Settings.candidates.wordnet, features, "trainingMelamud")
  
   // define lexsub system
   val lexsub = LexSubExpander(
      Settings.candidates.wordnet,
      features,
      SingleFeatureScorer())
      
    // eval pipeline
    val outcomes = lexsub(evaluationData)
    val results = Outcomes.collect(evaluationData, outcomes)
    
    SemEvalResultOutcomeWriter.save(results, "melamud.instances.out" )
    val best = Outcomes.evaluate(results, 1)
    val oot = Outcomes.evaluate(results, 10)
    println("best = %s\noot = %s".format(best, oot))
}