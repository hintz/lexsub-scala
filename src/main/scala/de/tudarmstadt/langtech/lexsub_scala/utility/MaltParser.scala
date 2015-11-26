package de.tudarmstadt.langtech.lexsub_scala.utility

import collection.JavaConversions._
import org.maltparser.concurrent.ConcurrentMaltParserService
import java.io.File
import java.net.URL
import org.maltparser.concurrent.ConcurrentMaltParserModel

/** Convenience wrapper around MaltParser */
class MaltParser(model: ConcurrentMaltParserModel) {
  def this(model: File) = this(ConcurrentMaltParserService.initializeParserModel(model))
  def this(url: URL) = this(ConcurrentMaltParserService.initializeParserModel(url))
  
  /** Returns depedeges as tuples (label, governorIndex, targetIndex) */
  def parse(tokens: Iterable[String], pos: Iterable[String]) = {
      val conll = tokens.zip(pos).zipWithIndex.map { case ((token, pos), i) => Seq(i+1, token, "_", pos, pos, "_").mkString("\t")}
      val graph = model.parse(conll.toArray)
      graph.getEdges.map { case e =>
        (e.getLabel("DEPREL"), e.getSource.getIndex - 1, e.getTarget.getIndex - 1)
      }
  }
}


object TestMalt extends App {

  
  val parser = new MaltParser(new File("resources/models/malt/engmalt.poly-1.7.mco"))
  val tokens = Seq("Pierre", "Vinken", ",", "61", "years", "old", ",", "will", "join", "the", "board", "as", "a", "nonexecutive", "director", "Nov.", "29", ".")
  val pos = Seq("NNP", "NNP", ",", "CD", "NNS", "JJ", ",", "MD", "VB", "DT", "NN", "IN", "DT", "JJ", "NN", "NNP", "CD", ".")

  parser.parse(tokens, pos) foreach println
}