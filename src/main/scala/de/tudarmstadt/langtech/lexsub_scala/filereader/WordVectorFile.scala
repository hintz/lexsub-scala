package de.tudarmstadt.langtech.lexsub_scala.filereader

import scalaz.Memo
import breeze.linalg.{Vector => Vector}
import breeze.linalg.DenseVector
import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile
class WordVectorFile(val embedding_file: String)  {
  
  val file = new PrefixIndexedFile(embedding_file, 10)
  
  /** Yields similar words based on this DT */
  def apply(word: String): Option[Vector[Double]] = repr(word)
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
  println(e("Welt"))
  println(e("Mars"))
  println(e("wüsste"))
  
  
}
