package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.lexsub_scala.germeval.LexItem
import de.tudarmstadt.langtech.scala_utilities.io
import org.apache.commons.lang.StringUtils
import de.tudarmstadt.langtech.lexsub_scala.Settings

object CreateDTCandidates extends App {
  
  val germevalGold = new GermEvalReader(Settings.germevalFolder, "train-dataset").gold.items
  val germevalLexItems = germevalGold.map(_.target).distinct
  
  val dtfile = Settings.dt1Similar
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