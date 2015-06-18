package de.tudarmstadt.langtech.lexsub_scala.distributional

import de.tudarmstadt.langtech.index_file.PrefixIndexedFile
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.utility.strings
import scalaz.Memo

class DT(val dt_filename: String, extractor: (String => Token)) {
  val file = new PrefixIndexedFile(dt_filename)
  
  type Result = Seq[(Token, Double)]
 
  //def lines(prefix: String) = file.search(prefix)
  
  /** Yields similar words based on this DT */
  def similar(s: String) = sim(s)
  val sim: String => Result = Memo.mutableHashMapMemo { prefix =>
    val lines = file.search(prefix)
    val processed = lines.map(_.split("\t")).map {
      case Array(_, other, score) => (extractor(other), score.toDouble)
    }
    processed.toSeq
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
  val dt = new DT("../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub", 
      HashtagSeparatedPos)
  
  dt.similar("wütend") foreach println
  
}