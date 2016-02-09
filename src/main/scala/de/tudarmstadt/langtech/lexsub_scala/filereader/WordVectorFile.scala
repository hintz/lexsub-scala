package de.tudarmstadt.langtech.lexsub_scala.filereader

import scalaz.Memo
import breeze.linalg.{Vector => Vector}
import breeze.linalg.DenseVector
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile
import de.tudarmstadt.langtech.scala_utilities.index_file.CachedPrefixIndexedFile
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.scala_utilities.cache.FileBackedCache

/** A simple reader for word vector files in the format [word] [SPACE] [floating-point vector].
 *  Internally uses PrefixIndexedFile which transparently builds an index */
class WordVectorFile(val embedding_file: String, useCache: Boolean = true)  {
  
  val file = new PrefixIndexedFile(embedding_file, 10)

  val lookup: String => Option[Vector[Double]] = Memo.mutableHashMapMemo { word =>
    val lines = file.search(word + " ") // make sure word is terminated by space!
    val v = lines.map(_.split(" ").toList).collectFirst {
      case `word` :: vector => vector.map(_.toDouble).toArray
    }
    v.map(DenseVector.apply)
  }
  
  val cache: String => Option[Vector[Double]] = 
    if(useCache) FileBackedCache(lookup, filename = LexsubUtil.getCachefile(embedding_file)).apply
    else lookup
  
  /** Yields similar words based on this DT */
  def apply(word: String): Option[Vector[Double]] = cache(word)
  
  override def toString = "WordVectorFile(%s)".format(io.filenamePart(embedding_file))
}
