package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordVectorFile
import de.tudarmstadt.langtech.scala_utilities._
import breeze.linalg.operators._
import breeze.linalg._
import breeze.numerics._
import breeze.linalg.functions.cosineDistance
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.feature.transform.extractor.CosineSimilarity
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.scala_utilities.formatting.Word2Vec
import breeze.linalg.DenseVector
import scalaz.Memo

/** Slim interface for looking up a word vector */
trait WordVectorLookup {
  def apply(word: String): Option[Vector[Double]]

  def similarity(f: (Vector[Double], Vector[Double]) => Double)(w1: String, w2: String): Option[Double] = {
    (apply(w1), apply(w2)) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _ => None
    }
  }
}

object LinAlgFunctions { 
  def distance(v1: Vector[Double], v2: Vector[Double]) = breeze.linalg.norm[Vector[Double], Double](v1 - v2)
  def cossim(v1: Vector[Double], v2: Vector[Double]) = breeze.linalg.functions.cosineDistance(v1, v2)
}

/** WordVectorLookup based on a plaintext file */
case class WordVectorFileLookup(filename: String) extends WordVectorFile(filename) with WordVectorLookup

/** WordVectorLookup based on Word2Vec binary data */
case class Word2VecLookup(filename: String, limit: Integer = Integer.MAX_VALUE) extends WordVectorLookup {
  lazy val file = new Word2Vec(filename, limit)
  
  val cache: String => Option[Vector[Double]] = Memo.mutableHashMapMemo { word =>
    val vec = file.vector(word).map(_.toDouble)
    Some(DenseVector(vec)).filter(_.length > 0)
  }
  
  def apply(word: String): Option[Vector[Double]] = cache(word)
}

case class WordEmbeddingSimilarity(val embedding: WordVectorLookup)
  extends LocalFeatureExtractor with NumericFeature {
  val name = "EmbeddingCosSim"

  private def cossim(v1: Vector[Double], v2: Vector[Double]) =
    breeze.linalg.functions.cosineDistance(v1, v2)

  def extract(item: SubstitutionItem): Seq[Feature] =
    embedding.similarity(cossim)(item.targetLemma, item.substitution)

}

/** the negative distance in embedding space */
case class WordEmbeddingDistance(val embedding: WordVectorLookup)
  extends LocalFeatureExtractor with NumericFeature {
  val name = "EmbeddingDist"
  private def distance(v1: Vector[Double], v2: Vector[Double]) =
    breeze.linalg.norm[Vector[Double], Double](v1 - v2)

  def extract(item: SubstitutionItem): Seq[Feature] =
    embedding.similarity(distance)(item.targetLemma, item.substitution).map(- _)
}

case class WordEmbeddingGlobalCache(originalHeadVector: Option[Vector[Double]], vectors: Seq[Option[Vector[Double]]])
case class WordEmbeddingDistanceVectors(embedding: WordVectorLookup, leftContext: Int, rightContext: Int)
  extends SmartFeature[WordEmbeddingGlobalCache] with NumericFeature {

  val name = "EmbeddingDist_" + leftContext + "_" + rightContext
  val slicer = collections.context[String](leftContext, rightContext) _

  def global(item: LexSubInstance): WordEmbeddingGlobalCache = {
    val sentence = item.sentence
    val tokens = sentence.tokens.map(_.word) // use word forms, not lemmas!
    val originalHeadVector = embedding(item.head.lemma) // use lemma, because we compare with lemmas
    val slice = slicer(tokens, item.headIndex)
    val vectors = slice.flatMap(t => t.map(embedding.apply))
    WordEmbeddingGlobalCache(originalHeadVector, vectors)
  }

  def extract(item: SubstitutionItem, global: WordEmbeddingGlobalCache): Seq[Feature] = {

    (global, embedding(item.substitution)) match {
      case (WordEmbeddingGlobalCache(Some(originalHeadVector), vectors), Some(substituteHeadVector)) =>

        val vectorsWithSubst = vectors.updated(leftContext, Some(substituteHeadVector))
        val origCos = vectors.map { v => v.map(LinAlgFunctions.cossim(_, originalHeadVector)) }
        val substCos = vectorsWithSubst.map { v => v.map(LinAlgFunctions.cossim(_, substituteHeadVector)) }
        
        val cosDeltas = origCos.zip(substCos).collect {
          case (Some(origCosSim), Some(newCosSim)) => Math.abs(origCosSim - newCosSim)
          case (Some(origCosSim), None) => Math.abs(origCosSim)
          case (None, Some(newCosSim)) => Math.abs(newCosSim)
          case (None, None) => 0d
        }
        val negativeDeltaSum = - cosDeltas.sum
        
        //println(item.targetLemma + " -> " + item.substitution)
        //println(cosDeltas)
        //println(negativeDeltaSum)
        
        negativeDeltaSum

        // Old idea: cosine similarity between diff vectors to each context word
        /*
        val origDeltas = vectors.map { v => v.map(_ - originalHeadVector) }
        val newDeltas = vectors.map { v => v.map(_ - substituteHeadVector) }

        val distances: Traversable[Double] = (origDeltas, newDeltas).zipped.collect {
          case (Some(origDelta), Some(newDelta)) =>
            //val deltaDelta = origDelta - newDelta
            //val difference = breeze.linalg.norm.apply[Vector[Double], Double](deltaDelta)
            val cossim = breeze.linalg.functions.cosineDistance(origDelta, newDelta)
            cossim
        }
        val result = distances.sum
        Some(result)
   
        */

      case other =>
         // either head or target not in embedding file!
        None
    }
  }
}

case class WordEmbeddingDistanceVectorsSet(embedding: WordVectorLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends Features((for (l <- leftRange; r <- rightRange; if l + r < maxSize) yield WordEmbeddingDistanceVectors(embedding, l, r)): _*)
