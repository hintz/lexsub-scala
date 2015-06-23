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

case class WordEmbeddingSimilarity(val embedding: WordVectorFile) 
  extends NumericOptionalValueFeatureExtractor[Null]("EmbeddingCosSim")
  with PureLocal {
  def extractOptValue(item: SubstitutionItem, global: Null): Option[Double] = 
    embedding.cossim(item.targetLemma, item.substitution)
}


case class WordEmbeddingDistance(val embedding: WordVectorFile) 
  extends NumericOptionalValueFeatureExtractor[Null]("EmbeddingDist")
  with PureLocal {
  
  private def distance(v1: Vector[Double], v2: Vector[Double]) = 
    breeze.linalg.norm[Vector[Double], Double](v1 - v2)
    
  def extractOptValue(item: SubstitutionItem, global: Null): Option[Double] = 
    embedding.similarity(distance)(item.targetLemma, item.substitution)
}


case class WordEmbeddingDistanceVectors(embedding: WordVectorFile, leftContext: Int, rightContext: Int) 
    extends NumericOptionalValueFeatureExtractor[Null]("EmbeddingDist_" + leftContext + "_" + rightContext)
    with PureLocal { 
  
  val slicer = utility.context[String](leftContext, rightContext) _
  
  def extractOptValue(item: SubstitutionItem, global: Null): Option[Double] = {
     val sentence = item.lexSubInstance.sentence
     val tokens = sentence.tokens.map(_.word) // use word forms, not lemmas!
     
     (embedding.vector(item.targetLemma), embedding.vector(item.substitution)) match {
       case (Some(originalHeadVector), Some(substituteHeadVector)) =>
         
         val slice = slicer(tokens, item.lexSubInstance.headIndex)
         val vectors = slice.flatMap(t => t.map(embedding.vector))
     
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