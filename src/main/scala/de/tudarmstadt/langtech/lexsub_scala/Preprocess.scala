package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalItem
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import java.io.File
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSTaggerME
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.types.Sentence
import java.util.IllegalFormatException
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import scala.util.Try

object Preprocessing {
  type Tokenizer = Function[String, Iterable[String]]
  type PosTagger = Function[Iterable[String], Iterable[String]]
  type Lemmatizer = Function[String, String]
}

case class Preprocessing(
  tokenizer: Preprocessing.Tokenizer,
  posTagger: Preprocessing.PosTagger,
  lemmatizer: Preprocessing.Lemmatizer) {
  
  def tryApply(goldItem: GermEvalItem): Option[LexSubInstance] = {
    val tried = Try(apply(goldItem))
    tried.recover { case e => System.err.println(e)}.toOption
    tried.toOption
  }

  def apply(goldItem: GermEvalItem): LexSubInstance = {
    val plaintext = goldItem.sentence.sentence
    val targetWord = goldItem.sentence.target.word

    val tokens = tokenizer(plaintext).toVector
    val postags = posTagger(tokens)
    val headIndex = tokens.indexWhere(_ == targetWord)
    if (headIndex < 0)
      throw new IllegalStateException("Could not find target word '%s' in tokenized sentence '%s'" format (targetWord, tokens))
    
    val lemmas = tokens.map(lemmatizer).toVector
      .updated(headIndex, goldItem.sentence.target.lemma) // override with gold lemma

    val sentenceTokens = (tokens, postags, lemmas).zipped.map { case (token, pos, lemma) => Token(token, pos, lemma) }
    val sentence = Sentence(sentenceTokens.toVector)


    LexSubInstance(sentence, headIndex, Some(goldItem))
  }
}