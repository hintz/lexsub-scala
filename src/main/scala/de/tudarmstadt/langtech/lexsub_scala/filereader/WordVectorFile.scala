package de.tudarmstadt.langtech.lexsub_scala.filereader

import scalaz.Memo
import breeze.linalg.{Vector => Vector}
import breeze.linalg.DenseVector
import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile

/** A simple reader for word vector files in the format [word] [SPACE] [floating-point vector].
 *  Internally uses PrefixIndexedFile which transparently builds an index */
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
