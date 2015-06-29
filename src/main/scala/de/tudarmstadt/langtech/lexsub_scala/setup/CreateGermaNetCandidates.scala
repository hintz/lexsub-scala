package de.tudarmstadt.langtech.lexsub_scala.setup

import de.tudarmstadt.langtech.scala_utilities._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tuebingen.uni.sfs.germanet.api.ConRel
import de.tuebingen.uni.sfs.germanet.api.Synset
import de.tuebingen.uni.sfs.germanet.api.WordCategory
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germanet.GermaNetUtils


object CreateGermaNetCandidates extends App {
  
  val gn = new GermaNet("AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  val gnUtils = new GermaNetUtils(gn)
  import gnUtils._
  
  def writeCandiadteFile(lexemes: Seq[(String, Option[WordCategory])], outfile: String){
      val out = new java.io.BufferedWriter(new java.io.FileWriter(outfile))
      for((lex, pos) <- lexemes; (other, relations) <- getSemanticRelations(lex, pos)) {
        val rel = relations.map("germanet_" + _).mkString(";")
        val pos2 = pos.map(translatePos).getOrElse("?")
        val line = Seq(lex, pos2, other, rel).mkString("\t")
        out.write(line + "\n")
        out.flush
      }
    out.close
  }
  
  val germevalGold = new GermEvalReader("AIPHES_Data/GermEval2015", "train-dataset").gold.items
  val germevalLexItems = germevalGold.map(_.target).distinct
  val germevalLexemes: Seq[(String, Option[WordCategory])] = germevalLexItems.map(x => (x.word, translatePos(x.pos)))
  writeCandiadteFile(germevalLexemes, "germeval_germanet")
  
  /*
  val lexemeSet: Set[(String, Option[WordCategory])] = 
    gn.getLexUnits.asScala
      .flatMap(lex => lex.getOrthForms.asScala
      .map((_, Some(lex.getWordCategory))))
      .toSet
  val lexemes = lexemeSet.toSeq.sortBy(_._1)    
  writeCandiadteFile(lexemes, "germanet_all2")
  */
}