package de.tudarmstadt.langtech.lexsub_scala.filereader

import de.tudarmstadt.langtech.scala_utilities.index_file.PrefixIndexedFile
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.scala_utilities.strings
import scalaz.Memo

class WordSimilarityFile[Elem](val dt_filename: String, extractor: (String => Elem), sorted: Boolean = true) {
  
  type Result = Seq[(Elem, Double)]
	
  // splitter splitting word / otherWord / score
  val Splitter = "\t"
  
  // the file backing this DT
  val file = new PrefixIndexedFile(dt_filename)
  
  /** Yields similar words based on this DT */
  def similar(s: String) = sim(s)
  val sim: String => Result = Memo.mutableHashMapMemo { prefix =>
    val lines = file.search(prefix)
    val processed = lines.map(_.split("\t")).flatMap {
      case Array(_, other, score) => Seq((extractor(other), score.toDouble))
      case line => 
        System.err.printf("WARNING: Illegal format in %s, searching for %s, parsed line %s\n", 
            dt_filename, prefix, line.mkString("\t"))
        Seq.empty
    }
    val result = processed.toList
    if(!sorted) processed else processed.sortBy(- _._2)
  }
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

object Test extends App {
  val dt = new WordSimilarityFile("../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub", 
      HashtagSeparatedPos)
  
  dt.similar("erleichterung") foreach println
  
}