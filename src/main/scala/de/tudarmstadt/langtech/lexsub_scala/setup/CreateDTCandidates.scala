package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.lexsub_scala.germeval.LexItem
import de.tudarmstadt.langtech.scala_utilities.io
import org.apache.commons.lang.StringUtils

object CreateDTCandidates extends App {
  
  val germevalGold = new GermEvalReader("../AIPHES_Data/GermEval2015", "train-dataset").gold.items
  val germevalLexItems = germevalGold.map(_.target).distinct
  
  val dtfile = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val dt = new WordSimilarityFile(dtfile, identity)
  
  val lines = for(
    LexItem(lemma, pos) <- germevalLexItems;
    val similar = dt.similar(lemma.toLowerCase + "#" + pos.toUpperCase);
    ((entry, score), idx) <- similar.zipWithIndex) yield {
      val otherLemma = entry.takeWhile(_ != '#')
      val other = if(pos == "n") StringUtils.capitalize(otherLemma) else otherLemma // hack
      val line = Seq(lemma, pos, other, "DT_" + idx).mkString("\t")
      line
  }

  io.write("germeval_DT_de_mate_lemma.tsv", lines.mkString("\n"))
}