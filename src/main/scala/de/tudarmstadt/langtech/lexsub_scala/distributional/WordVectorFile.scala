package de.tudarmstadt.langtech.lexsub_scala.distributional

import de.tudarmstadt.langtech.index_file.PrefixIndexedFile
import scalaz.Memo

class WordVectorFile(val embedding_file: String)  {
  
  val file = new PrefixIndexedFile(embedding_file)
  
  def cossim(w1: String, w2: String): Option[Double] = {
    def dot(v1: Vector[Double], v2: Vector[Double]) = (v1, v2).zipped.map(_ * _).sum
    def norm(v: Vector[Double]): Double = math.sqrt(dot(v, v))
    (vector(w1), vector(w2)) match {
      case (Some(v1), Some(v2)) => 
        Some(dot(v1, v2) / (norm(v1) * norm(v2)))
      case _ => None
    }
  }
  
  /** Yields similar words based on this DT */
  def vector(word: String): Option[Vector[Double]] = repr(word)
  val repr: String => Option[Vector[Double]] = Memo.mutableHashMapMemo { word =>
    val lines = file.search(word)
    lines.map(_.split(" ").toList).collectFirst {
      case `word` :: vector => vector.map(_.toDouble).toVector
    }
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
