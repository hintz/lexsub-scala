package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalItem
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.scala_utilities.io
import scala.util.Try


/** Very slim interfaces for preprocessing! */
object Preprocessing {
  type Tokenizer = String => Iterable[String]
  type PosTagger = Iterable[String] => Iterable[String]
  type Lemmatizer = String => String
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
  
  def loadGermEval(germevalFolder: String, filename: String): Seq[LexSubInstance] = {
     println("Loading GermEval data..")
     io.lazySerialized("cache_%s.ser".format(filename)){
      System.err.println("Cache does not exist, preprocessing GermEval data..")
      val plainData = new GermEvalReader(germevalFolder, filename).items
      val processed = plainData.flatMap(tryApply)
      processed
    }
  }
}