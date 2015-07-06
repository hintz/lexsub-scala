package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes

object RunGermevalEmbeddings extends App {
  
  val trainingData = Settings.germevalTraining
  val evaluationData = Settings.germevalTraining
  
  val cosSim = WordEmbeddingSimilarity(Settings.word2vecEmbedding)
  
  // load lexsub system
  val lexsub = LexSubExpander(
      Settings.candidates.germanet,
      new FeatureAnnotator(cosSim),
      SingleFeatureScorer(cosSim.name))

  val outcomes = lexsub(trainingData)
  
  // write results
  val results = Outcomes.collect(trainingData, outcomes)
  GermEvalResultOutcomeWriter.save(results, Settings.instancesOutputFile)
  
  val best = Outcomes.evaluate(results, 1)
  val oot = Outcomes.evaluate(results, 1)
  println("best = %s\noot = %s".format(best, oot))
}