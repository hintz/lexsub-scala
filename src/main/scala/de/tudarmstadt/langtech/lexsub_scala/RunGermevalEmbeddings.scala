package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.features.WordVectorLookup
import breeze.linalg._
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistance
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistanceVectors
import de.tudarmstadt.langtech.lexsub_scala.features.SummedWordEmbeddingDistances

object RunGermevalEmbeddings extends App {
  
	private def cossim(v1: Vector[Double], v2: Vector[Double]) = breeze.linalg.functions.cosineDistance(v1, v2)
  
  //val embedding: WordVectorLookup = Settings.embeddings.word2vec
  val embedding: WordVectorLookup = Settings.embeddings.eigenword
  
  val trainingData = Settings.germevalTraining
  
  val cosSim = WordEmbeddingSimilarity(embedding) //SummedWordEmbeddingDistances(embedding, 0 to 2, 0 to 2, 5) // WordEmbeddingDistanceVectors(embedding, 1, 1) //WordEmbeddingSimilarity(embedding)
  
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
  val oot = Outcomes.evaluate(results, 10)
  println("best = %s\noot = %s".format(best, oot))
}