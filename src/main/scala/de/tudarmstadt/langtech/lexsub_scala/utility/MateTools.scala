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

  val tagger = taggerModel.map(new Tagger(_))
  val lemmatizer = lemmatizerModel.map(new Lemmatizer(_))
  val parser = parserModel.map(new Parser(_))

  def apply(s: String): Sentence = {

    val tokens = Array("<root>") ++ tokenizer(s)

    var i = new SentenceData09
    i.init(tokens)

    tagger.foreach { tagger => i = tagger(i) }
    lemmatizer.foreach { lemmatizer => i = lemmatizer(i) }
    parser.foreach { parser => i = parser(i) }

    println(i)

    val out = (i.forms, i.ppos, i.plemmas).zipped.map {
      case (form, pos, lemma) => Token(form, pos, lemma)
    }

    val realTokens = tokens.tail
    val deps = i.plabels.zip(i.pheads).zipWithIndex map {
      case ((label, from), to) =>
        println(label + "(" + tokens(from) + ", " + realTokens(to) + ")")
        DepEdge(label, from - 1, to)
    }

    println(deps.toVector)

    Sentence(out.toVector)
  }

}

object MateToolsTest {

  val Models = Map(
    "en" -> Map(
      "tagger" -> "resources/models/mate/tagger-eng-4M-v36.mdl",
      "lemmatizer" -> "resources/models/mate/lemmatizer-eng-4M-v36.mdl",
      "parser" -> "resources/models/mate/parser-eng-12M-v36.mdl"),
    "de" -> Map(
      "lemmatizer" -> "resources/models/mate/lemma-ger-3.6.model",
      "parser" -> "resources/models/mate/parser-ger-3.6.model"))

  def main(args: Array[String]) {

    val german = Array("<root>", "Peter", "hat", "eine", "Katze", ",", "die", "gerne", "Mäuse", "fängt", ".")

    val mate = MateProcessing(_.split(" "),
      taggerModel = Some(Models("en")("tagger")),
      lemmatizerModel = Some(Models("en")("lemmatizer")),
      parserModel = Some(Models("en")("parser")))

    println(mate("Peter has a cat who like catching mice ."))

  }
}