package de.tudarmstadt.langtech.lexsub_scala

import de.tudarmstadt.langtech.lexsub_scala.types.NLPPipeline
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.processing._
import de.tudarmstadt.langtech.scala_utilities.processing.ReportingIterable._
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import scala.util.Try

class LexSubProcessing(preprocessing: NLPPipeline) extends BatchProcessing[SemEvalItem, LexSubInstance] {
  
  /** Parses SemEval data */
  def parseSemEval(data: Seq[SemEvalItem]) = data.reporting(report, 5000).flatMap(tryApply).toSeq
  
  /** Parses a single SemEvalItem wrapping any exceptions in Option */
  def tryApply(goldItem: SemEvalItem): Option[LexSubInstance] = {
    val tried = Try(apply(goldItem))
    tried.recover { case e => System.err.println(e) }.toOption
    tried.toOption
  }
  
  /** Parses a single SemEvalItem */
  def apply(goldItem: SemEvalItem): LexSubInstance = {
    val plaintext = goldItem.sentence.sentence
    val targetWord = goldItem.sentence.target.word
    val parsed = preprocessing(plaintext)
    val forms = parsed.tokens.map(_.word)
    
    // determine index of head word
    val headIndex = forms.indexWhere(_ == targetWord)
    if (headIndex < 0)
      throw new IllegalStateException("Could not find target word '%s' in tokenized sentence '%s'" format (targetWord, forms))
    
    // overwrite parsed lemma with gold lemma
    val updatedLemma = parsed.tokens(headIndex).copy(lemma = goldItem.sentence.target.lemma)
    val updatedTokens = parsed.tokens.updated(headIndex, updatedLemma)
    val sentence = parsed.copy(tokens = updatedTokens)

    LexSubInstance(sentence, headIndex, Some(goldItem))
  }
}


object LexSubProcessing {
  implicit def withPipeline(pipeline: NLPPipeline): LexSubProcessing = new LexSubProcessing(pipeline)
}