package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tuebingen.uni.sfs.germanet.api.WordCategory
import de.tudarmstadt.langtech.lexsub_scala.germeval.LexItem

object CreateDTCandidates extends App {
  
  val germevalGold = new GermEvalReader("../AIPHES_Data/GermEval2015", "train-dataset").gold.items
  val germevalLexItems = germevalGold.map(_.target).distinct
  
  val dtfile = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val dt = new WordSimilarityFile(dtfile, identity)
  
  for(LexItem(lemma, pos) <- germevalLexItems){
    val similar = dt.similar(lemma.toLowerCase + "#" + pos.toUpperCase)
    for(((entry, score), idx) <- similar.zipWithIndex){
      val other = entry.takeWhile(_ != '#')
      val line = Seq(lemma, pos, other, "DT_de70_mate_lemma_" + idx).mkString("\t")
      println(line)
    }
  }
}