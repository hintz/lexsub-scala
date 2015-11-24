package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing

/** Default interface for pre-processing to implement */
trait NLPPipeline {
  def apply(sentence: String): Sentence
}

/** Very slim interfaces for elementary preprocessing! */
object SimpleProcessing {
  type Tokenizer = String => Iterable[String]
  type PosTagger = Iterable[String] => Iterable[String]
  type Lemmatizer = String => String
}

case class SimpleProcessing(
  tokenize: SimpleProcessing.Tokenizer,
  posTag: SimpleProcessing.PosTagger,
  lemmatize: SimpleProcessing.Lemmatizer) extends LexSubProcessing(new NLPPipeline {
  def apply(sentence: String) = {
    val tokens = tokenize(sentence)
    val tags = posTag(tokens)
    val lemmas = tokens.map(lemmatize)
    val output = (tokens, tags, lemmas).zipped.map { case (form, tag, lemma) => Token(form, tag, lemma) }
    Sentence(output.toVector)
  }
})