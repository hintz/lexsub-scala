package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.utility._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tuebingen.uni.sfs.germanet.api.ConRel
import de.tuebingen.uni.sfs.germanet.api.Synset
import de.tuebingen.uni.sfs.germanet.api.WordCategory


object CreateGermaNetCandidates extends App {
  
  val Outfile = "germanet_all"
  val gn = new GermaNet("AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  val gnUtils = new GermaNetUtils(gn)
  import gnUtils._
  
  val units = gn.getLexUnits.asScala
  val lexemes: Seq[(String, WordCategory)] =
    units.flatMap(lex => lex.getOrthForms.asScala.map((_, lex.getWordCategory))).toSet.toSeq.sorted
  
  val out = new java.io.BufferedWriter(new java.io.FileWriter(Outfile))
  for((lex, pos) <- lexemes; (other, relations) <- getSemanticRelations(lex, pos)) {
    val rel = relations.map("germanet_" + _).mkString(";")
    val line = Seq(lex, simplifyPos(pos), other, rel).mkString("\t")
    out.write(line + "\n")
    out.flush
  }

  out.close
  println("Done writing germanet candidates")
}