package de.tudarmstadt.langtech.lexsub_scala.types

case class Candidate(
    /** the lexical item to be replaced */
    val word: String, 
    /** Part-of-Speech */
    val pos: String, 
    /** the replacement word */
    val replacement: String, 
    /** optional set of semantic relation labels extracted from one or more semantic resources */
    val relations: Set[String] = Set.empty) {
  def identity = (word, pos, replacement)
}


/** A function yielding a set of lexical substitution candidates */
trait CandidateFunction {
  def get(lemma: String): Seq[Candidate]
  
  /** Convenience function to get candidate lemmas */
  def apply(lemma: String) = get(lemma).map(_.replacement)
}