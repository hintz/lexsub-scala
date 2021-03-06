package de.tudarmstadt.langtech.lexsub_scala.features

import breeze.linalg.DenseVector
import breeze.linalg.Vector
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordVectorFile
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities.collections
import de.tudarmstadt.langtech.scala_utilities.compute
import de.tudarmstadt.langtech.scala_utilities.formatting.Word2Vec
import scalaz._
import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

/** Slim interface for looking up a word vector */
trait WordVectorLookup {
  def apply(word: String): Option[Vector[Double]]

  def similarity(f: (Vector[Double], Vector[Double]) => Double)(w1: String, w2: String): Option[Double] = {
    (apply(w1), apply(w2)) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _ => None
    }
  }
  def isLowercased: Boolean
}

// Some wrappers for breeze. Shouldn't be necessary if I understood the breeze typing system correctly..
object LinAlgFunctions { 
  def distance(v1: Vector[Double], v2: Vector[Double]) = breeze.linalg.norm[Vector[Double], Double](v1 - v2)
  def cossim(v1: Vector[Double], v2: Vector[Double]) = 1d - breeze.linalg.functions.cosineDistance(v1, v2)
}

/** WordVectorLookup based on a plaintext file */
case class WordVectorFileLookup(filename: String, isLowercased: Boolean = false) 
  extends WordVectorFile(filename) with WordVectorLookup

/** WordVectorLookup based on Word2Vec binary data */
case class Word2VecLookup(filename: String, limit: Integer = Integer.MAX_VALUE, isLowercased: Boolean = false) 
  extends WordVectorLookup {
  lazy val file = new Word2Vec(filename, limit)
  lazy val cache = FileBackedCache(lookup, LexsubUtil.getCachefile(filename))
  
  private def lookup(word: String): Option[Vector[Double]] = {
    val vec = file.vector(word).map(_.toDouble)
    Some(DenseVector(vec)).filter(_.length > 0)
  }
  def apply(word: String): Option[Vector[Double]] = cache(word)
  
  override def toString = "Word2VecLookup(%s)".format(filename.replaceAll("""[\/\\]+""", "_"))
}

/** the negative cosine similarity in embedding space */
case class WordEmbeddingSimilarity(val embedding: WordVectorLookup)
  extends LocalFeatureExtractor with FeatureUtils {
  implicit val name = "EmbeddingCosSim"
  
  def extract(item: SubstitutionItem): Seq[Feature] =
    embedding.similarity(LinAlgFunctions.cossim)(item.targetLemma, item.substitution).map(- _)
}

/** the negative distance in embedding space */
case class WordEmbeddingDistance(val embedding: WordVectorLookup)
  extends LocalFeatureExtractor with FeatureUtils {
  implicit  val name = "EmbeddingDist"

  def extract(item: SubstitutionItem): Seq[Feature] =
    embedding.similarity(LinAlgFunctions.distance)(item.targetLemma, item.substitution).map(- _)
}

case class WordEmbeddingGlobalCache(originalHeadVector: Option[Vector[Double]], vectors: Seq[Option[Vector[Double]]])
case class WordEmbeddingDistanceVectors(embedding: WordVectorLookup, leftContext: Int, rightContext: Int)
  extends SmartFeature[WordEmbeddingGlobalCache] with FeatureUtils {

  implicit val name = "EmbeddingDist_" + leftContext + "_" + rightContext
  val slicer = collections.context[String](leftContext, rightContext) _

  def global(item: LexSubInstance): WordEmbeddingGlobalCache = {
    val sentence = item.sentence
    val tokens = sentence.tokens.map(_.word) // use word forms, not lemmas!
    val originalHeadVector = embedding(item.head.lemma) // use lemma, because we compare with lemmas
    
    // extract token slice, reject if out of sentence bounds..
    val slice = slicer(tokens, item.headIndex).map(_.getOrElse { return WordEmbeddingGlobalCache(None, Seq.empty)})
    
    // map to embedding space
    val vectors = slice.map(embedding.apply)
    
    WordEmbeddingGlobalCache(originalHeadVector, vectors)
  }

  def extract(item: SubstitutionItem, global: WordEmbeddingGlobalCache): Seq[Feature] = {

    (global, embedding(item.substitution)) match {
      case (WordEmbeddingGlobalCache(target @ Some(originalHeadVector), vectors), Some(substituteHeadVector)) =>

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

      case other =>
         // either head or target not in embedding file!
        None
    }
  }
}

/** Generates WordEmbeddingDistance features for all given context windows */
case class WordEmbeddingDistanceVectorsSet(embedding: WordVectorLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends Features((for (l <- leftRange; r <- rightRange; if l + r < maxSize) yield WordEmbeddingDistanceVectors(embedding, l, r)): _*)

/** Sums the WordEmbeddingDistance features for all given context windows */
case class SummedWordEmbeddingDistances(embedding: WordVectorLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends AggregatedFeature("SummedWordEmbeddingDistances", WordEmbeddingDistanceVectorsSet(embedding, leftRange, rightRange, maxSize), _.sum) 

/** Aggregates EmbeddingSimilarity with context distances, similar to (Melamud et. al, 2015) */
case class MeanEmbeddingSimilarityAndContextDistance(embedding: WordVectorLookup, leftRange: Range, rightRange: Range, maxSize: Int)
  extends AggregatedFeature("MeanEmbSimContextDist", new Features(
      SummedWordEmbeddingDistances(embedding, leftRange, rightRange, maxSize), 
      WordEmbeddingSimilarity(embedding)), compute.mean)
