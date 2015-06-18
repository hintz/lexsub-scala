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

// TODO
trait Tokenizer
trait PosTagger
trait Lemmatizer

object Preprocess {
  
  val opennlpTokenizer = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
  val postagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))

  def apply(goldItem: GermEvalItem): LexSubInstance = {
    val plaintext = goldItem.sentence.sentence
    val targetWord = goldItem.sentence.target.word
    
    val tokens = opennlpTokenizer.tokenize(plaintext)
    val postags = postagger.tag(tokens)
    val headIndex = tokens.indexWhere(_ == targetWord)
    
    // TODO: override lemma
    val lemmas = Iterator.continually("?").take(tokens.length).toVector
      .updated(headIndex, goldItem.sentence.target.lemma) // override with gold lemma
    
    val sentenceTokens = (tokens, postags, lemmas).zipped.map { case (token, pos, lemma) => Token(token, pos, lemma)}
    val sentence = Sentence(sentenceTokens.toVector)
    
    if(headIndex < 0)
      throw new IllegalStateException("Could not find target word %s in sentence '%s'" format (targetWord, plaintext))
    
    LexSubInstance(sentence, headIndex, Some(goldItem))
  }
  
}