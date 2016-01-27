package de.tudarmstadt.langtech.lexsub_scala.utility

import is2.data.SentenceData09
import is2.lemmatizer.Lemmatizer
import is2.parser.Parser
import is2.tag.Tagger
import de.tudarmstadt.langtech.lexsub_scala.types.NLPPipeline
import de.tudarmstadt.langtech.lexsub_scala.types.Sentence
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge

case class MateProcessing(
  tokenizer: String => Iterable[String],
  taggerModel: Option[String],
  lemmatizerModel: Option[String],
  parserModel: Option[String]) extends NLPPipeline {

  @transient lazy val tagger = taggerModel.map(new Tagger(_))
  @transient lazy val lemmatizer = lemmatizerModel.map(new Lemmatizer(_))
  @transient lazy val parser = parserModel.map(new Parser(_))

  def apply(s: String): Sentence = {

    val tokens = Array("<root>") ++ tokenizer(s)

    var i = new SentenceData09
    i.init(tokens)

    tagger.foreach { tagger => i = tagger(i) }
    lemmatizer.foreach { lemmatizer => i = lemmatizer(i) }
    parser.foreach { parser => i = parser(i) }

    //println(i)

    val out = (i.forms, i.ppos, i.plemmas).zipped.map {
      case (form, pos, lemma) => Token(form, pos, lemma)
    }

    val realTokens = tokens.tail
    val deps = i.plabels.zip(i.pheads).zipWithIndex map {
      case ((label, from), to) =>
        //println(label + "(" + tokens(from) + ", " + realTokens(to) + ")")
        DepEdge(label, from - 1, to)
    }

    Sentence(out.toVector, deps.toList)
  }
}

object TestMateTools {

  val Models = Map(
    "it" -> Map(
      "tagger" -> "resources/models/mate/tagger-it-3.6.model",
      "lemmatizer" -> "resources/models/mate/lemmatizer-it-3.6.model",
      "parser" -> "resources/models/mate/parser-it-3.6.model")
  )

  def main(args: Array[String]) {

    val mate = MateProcessing(_.split(" "),
      taggerModel = Some(Models("it")("tagger")),
      lemmatizerModel = Some(Models("it")("lemmatizer")),
      parserModel = Some(Models("it")("parser")))

    //println(mate("Peter has a cat who likes catching mice ."))
    println(mate("È stata la giornata di il doppio oro italiano a i Mondiali di atletica."))

  }
}