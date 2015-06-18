package de.tudarmstadt.langtech.lexsub_scala.distributional

import de.tudarmstadt.langtech.index_file.PrefixIndexedFile

class DT(val dt_filename: String) {
  val file = new PrefixIndexedFile(dt_filename)
  
  
  def lines(prefix: String) = file.search(prefix)
}



object Test extends App {
  val dt = new DT("../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub")
  
  dt.lines("wü") foreach println
  
}