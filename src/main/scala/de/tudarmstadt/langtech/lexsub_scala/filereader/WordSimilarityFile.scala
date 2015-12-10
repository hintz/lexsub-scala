package de.tudarmstadt.langtech.lexsub_scala.filereader

import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile
import de.tudarmstadt.langtech.scala_utilities.index_file.CachedPrefixIndexedFile
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.scala_utilities.{strings, io}
import scalaz.Memo

/** A simple reader for sorted files in the format [word] \t [somethingElse] \t [score].
 *  Internally uses PrefixIndexedFile which transparently builds an index */
class WordSimilarityFile[Elem](val dt_filename: String, 
    extractor: (String => Elem), 
    sorted: Boolean = true, // if the output should be sorted
    matchPrefix: Boolean = false) // if entries are looked up only by prefix
{
  type Result = Seq[(Elem, Double)]
	
  // splitter splitting word / otherWord / score
  val Splitter = "\t"
  
  // the file backing this DT
  val file = new CachedPrefixIndexedFile(dt_filename)
  
  /** Yields similar words based on this DT */
  def similar(s: String) = sim(s)
  val sim: String => Result = Memo.mutableHashMapMemo { prefix =>
    val lines = file.search(prefix)
    val processed = lines.map(_.split("\t")).flatMap {
      case Array(elem, other, score) if matchPrefix || elem == prefix => 
        Seq((extractor(other), score.toDouble))
      case Array(_, _, _) => Seq.empty
      case line => 
        System.err.printf(
            "WARNING: Illegal format in %s, searching for %s, parsed line %s\n", 
            dt_filename, prefix, line.mkString("\t"))
        Seq.empty
    }
    val result = processed.toList
    if(!sorted) processed else processed.sortBy(- _._2)
  }
  
  override def toString = "WordSimilarityFile(%s)".format(io.filenamePart(dt_filename))
}

object PlainLemma extends Function[String, Token] {
  def apply(word: String): Token = Token("","", word)
}

object HashtagSeparatedPos extends Function[String, Token] {
  def apply(word: String): Token = {
    val (token, pos) = strings.splitAssign('#')(word)
    Token(token, pos, token)
  }
}