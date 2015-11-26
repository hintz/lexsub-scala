package de.tudarmstadt.langtech.lexsub_scala.utility

import collection.JavaConversions._
import org.maltparser.concurrent.ConcurrentMaltParserService
import java.io.File
import java.net.URL
import org.maltparser.concurrent.ConcurrentMaltParserModel
import de.tudarmstadt.langtech.lexsub_scala.types.NLPPipeline
import de.tudarmstadt.langtech.lexsub_scala.types.Sentence
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge


/** Convenience wrapper around MaltParser */
class MaltParser(model: ConcurrentMaltParserModel) {
  
  /* Set to true to keep unlabeled "null" edges */
  val KeepNullEdges = false
  
  def this(model: File) = this(ConcurrentMaltParserService.initializeParserModel(model))
  def this(url: URL) = this(ConcurrentMaltParserService.initializeParserModel(url))
  
  /** Returns depedeges as tuples (label, governorIndex, targetIndex) */
  def parse(tokens: Iterable[String], pos: Iterable[String]) = {
      val conll = tokens.zip(pos).zipWithIndex.map { case ((token, pos), i) => Seq(i+1, token, "_", pos, pos, "_").mkString("\t")}
      val graph = model.parse(conll.toArray)
      val result = graph.getEdges.map { case e =>
        val label  = e.getLabel("DEPREL")
        (label, e.getSource.getIndex - 1, e.getTarget.getIndex - 1)
      }
      if(KeepNullEdges) result
      else result.filter(_._1 != "null")
  }
}

case class MaltProcessing(
  tokenizer: String => Iterable[String],
  tagger: Iterable[String] => Iterable[String],
  lemmatizer: String => String,
  maltModel: String) extends NLPPipeline {
  
  val malt = new MaltParser(new File(maltModel))
  
  def apply(s: String): Sentence = {
    val tokens = tokenizer(s)
    val pos = tagger(tokens)
    val lemmas = tokens map lemmatizer
    val edges = malt.parse(tokens, pos)
    val depedges = edges.map(DepEdge.tupled)
    val output = (tokens, pos, lemmas).zipped.map(Token.apply)
    Sentence(output.toVector, depedges.toList)
  }
}


object TestMalt extends App {
  
  import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
  import opennlp.tools.postag.POSTaggerME
  import opennlp.tools.postag.POSModel
  
  val malt = MaltProcessing(
    tokenizer = _.split(" "), 
    tagger = new SimpleProcessing.PosTagger {
      lazy val tagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/en-pos-perceptron.bin")))
      def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
    },
    lemmatizer = identity,
    maltModel = "resources/models/malt/engmalt.poly-1.7.mco")
    
  println(malt("Peter has a cat who like to catch mice ."))

  //val parser = new MaltParser(new File("resources/models/malt/engmalt.poly-1.7.mco"))
  //val tokens = Seq("Pierre", "Vinken", ",", "61", "years", "old", ",", "will", "join", "the", "board", "as", "a", "nonexecutive", "director", "Nov.", "29", ".")
  //val pos = Seq("NNP", "NNP", ",", "CD", "NNS", "JJ", ",", "MD", "VB", "DT", "NN", "IN", "DT", "JJ", "NN", "NNP", "CD", ".")
  //println(parser.parse(tokens, pos))
}