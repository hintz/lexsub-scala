package de.tudarmstadt.langtech.lexsub_scala.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.features.WordVectorLookup
import breeze.linalg._
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistance
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistanceVectors
import de.tudarmstadt.langtech.lexsub_scala.features.SummedWordEmbeddingDistances
import de.tudarmstadt.langtech.lexsub_scala.features.MeanEmbeddingSimilarityAndContextDistance
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.SingleFeatureScorer

object RunGermevalEmbeddings extends App {
  
  
	val trainingData = Settings.germevalTraining
  
  /*
  //val embedding: WordVectorLookup = Settings.embeddings.word2vec
  //val embedding: WordVectorLookup = Settings.embeddings.eigenword
  
  val cosSim = SummedWordEmbeddingDistances(embedding, 0 to 2, 0 to 2, 5) //WordEmbeddingSimilarity(embedding) //  WordEmbeddingDistanceVectors(embedding, 1, 1) //WordEmbeddingSimilarity(embedding)
  
  val lexsub = LexSubExpander(
      Settings.candidates.germanet,
      new FeatureAnnotator(cosSim),
      SingleFeatureScorer())

  val outcomes = lexsub(trainingData)
  
  val results = Outcomes.collect(trainingData, outcomes)
  SemEvalResultOutcomeWriter.save(results, Settings.instancesOutputFile)
  
  val best = Outcomes.evaluate(results, 1)
  val oot = Outcomes.evaluate(results, 10)
  println("best = %s\noot = %s".format(best, oot))
  */
  
  for((embeddingName, embedding) <- Seq(
       ("word2vec", Settings.embeddings.word2vec),
       ("eigenword", Settings.embeddings.eigenword)
      );
      feat <- Seq(
      SummedWordEmbeddingDistances(embedding, 0 to 2, 0 to 2, 5),
      MeanEmbeddingSimilarityAndContextDistance(embedding, 0 to 2, 0 to 2, 5),
      WordEmbeddingSimilarity(embedding),
      WordEmbeddingDistanceVectors(embedding, 1, 1),
      WordEmbeddingSimilarity(embedding))) {
    val experimentName = embeddingName + feat.getClass.getSimpleName
    new java.io.File(experimentName).mkdirs
    
    val lexsub = LexSubExpander(
      Settings.candidates.germanet,
      new FeatureAnnotator(feat),
      SingleFeatureScorer())

    val outcomes = lexsub(trainingData)
    val results = Outcomes.collect(trainingData, outcomes)
    SemEvalResultOutcomeWriter.save(results, experimentName + "/instances.out" )
    val best = Outcomes.evaluate(results, 1)
    val oot = Outcomes.evaluate(results, 10)
    println("best = %s\noot = %s".format(best, oot))
  }
}

