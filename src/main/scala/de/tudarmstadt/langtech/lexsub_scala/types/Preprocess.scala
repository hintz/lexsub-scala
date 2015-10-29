package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
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
  
  def tryApply(goldItem: SemEvalItem): Option[LexSubInstance] = {
    val tried = Try(apply(goldItem))
    tried.recover { case e => System.err.println(e)}.toOption
    tried.toOption
  }

  def apply(goldItem: SemEvalItem): LexSubInstance = {
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
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file */
  def preprocessSemEval(folder: String, datafile: String, goldfile: String): Seq[LexSubInstance] = {
     val cachefile = "cache_%s.ser".format((folder + "-" + datafile).replaceAll("""[\/\.]+""","-"))
     io.lazySerialized(cachefile){
      System.err.println("Cachefile does not exist, preprocessing SemEval data..")
      val plainData = new SemEvalReader(folder, datafile, goldfile).items
      val processed = plainData.flatMap(tryApply)
      processed
    }
  }
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file
   *  (default naming convention)
   */
  def preprocessSemEval(germevalFolder: String, filename: String): Seq[LexSubInstance] = 
    preprocessSemEval(germevalFolder, filename + ".xml", filename + ".gold")
  
}