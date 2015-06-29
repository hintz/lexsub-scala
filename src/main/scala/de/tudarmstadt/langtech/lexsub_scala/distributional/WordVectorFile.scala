package de.tudarmstadt.langtech.lexsub_scala.distributional

import scalaz.Memo
import breeze.linalg.{Vector => Vector}
import breeze.linalg.DenseVector
import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile
class WordVectorFile(val embedding_file: String)  {
  
  val file = new PrefixIndexedFile(embedding_file, 10)
  
  def cossim(w1: String, w2: String) = similarity(breeze.linalg.functions.cosineDistance(_, _))(w1, w2)
  def similarity(f: (Vector[Double], Vector[Double]) => Double)(w1: String, w2: String): Option[Double] = {
    (vector(w1), vector(w2)) match {
      case (Some(v1), Some(v2)) => 
        val result = f(v1, v2)
        Some(result)
      case _ => None
    }
  }
  
  /** Yields similar words based on this DT */
  def vector(word: String): Option[Vector[Double]] = repr(word)
  val repr: String => Option[Vector[Double]] = Memo.mutableHashMapMemo { word =>
    val lines = file.search(word)
    val v = lines.map(_.split(" ").toList).collectFirst {
      case `word` :: vector => vector.map(_.toDouble).toArray
    }
    v.map(DenseVector.apply)
  }
}

object TestEmbedding extends App {
  val e = new WordVectorFile("../lexsub-gpl/AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted")
  println(e.vector("Welt"))
  println(e.vector("Mars"))
  println(e.vector("wüsste"))
  
  println(e.cossim("Welt", "Mars"))
  println(e.cossim("Hund", "Katze"))
  println(e.cossim("Tür", "versichert"))
  
}
