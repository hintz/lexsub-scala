package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordVectorFile
import de.tudarmstadt.langtech.lexsub_scala.utility
import breeze.linalg.operators._
import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.cosineDistance
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.feature.transform.extractor.CosineSimilarity
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance

case class WordEmbeddingSimilarity(val embedding: WordVectorFile)
extends LocalFeatureExtractor with NumericFeature {
  val name = "EmbeddingCosSim"
  def extract(item: SubstitutionItem): Seq[Feature] = 
    embedding.cossim(item.targetLemma, item.substitution)
}


case class WordEmbeddingDistance(val embedding: WordVectorFile)
extends LocalFeatureExtractor with NumericFeature {
  val name = "EmbeddingDist"
  private def distance(v1: Vector[Double], v2: Vector[Double]) = 
    breeze.linalg.norm[Vector[Double], Double](v1 - v2)
    
  def extract(item: SubstitutionItem): Seq[Feature] = 
    embedding.similarity(distance)(item.targetLemma, item.substitution)
}

case class WordEmbeddingGlobalCache(originalHeadVector: Option[Vector[Double]], vectors: Seq[Option[Vector[Double]]])
case class WordEmbeddingDistanceVectors(embedding: WordVectorFile, leftContext: Int, rightContext: Int) 
extends SmartFeature[WordEmbeddingGlobalCache] with NumericFeature {

  val name = "EmbeddingDist_" + leftContext + "_" + rightContext
  val slicer = utility.context[String](leftContext, rightContext) _
  
  def global(item: LexSubInstance): WordEmbeddingGlobalCache = {
    val sentence = item.sentence
     val tokens = sentence.tokens.map(_.word) // use word forms, not lemmas!
     val originalHeadVector = embedding.vector(item.head.lemma) // use lemma, because we compare with lemmas
     val slice = slicer(tokens, item.headIndex)
     val vectors = slice.flatMap(t => t.map(embedding.vector))
     WordEmbeddingGlobalCache(originalHeadVector, vectors)
  }
  
  def extract(item: SubstitutionItem, global: WordEmbeddingGlobalCache): Seq[Feature] =  {

     (global, embedding.vector(item.substitution)) match {
       case (WordEmbeddingGlobalCache(Some(originalHeadVector), vectors), Some(substituteHeadVector)) =>
         val origDeltas = vectors.map { v => v.map(_ - originalHeadVector) }
         val newDeltas = vectors.map  { v => v.map(_ - substituteHeadVector) }
         
         val distances: Traversable[Double] = (origDeltas, newDeltas).zipped.collect { 
           case (Some(origDelta), Some(newDelta)) => 
             //val deltaDelta = origDelta - newDelta
             //val difference = breeze.linalg.norm.apply[Vector[Double], Double](deltaDelta)
             val cossim = breeze.linalg.functions.cosineDistance(origDelta, newDelta)
             cossim
         }
         val result = distances.sum
         Some(result)

       case _ => None // either head or target not in embedding file!
     }
  }
}