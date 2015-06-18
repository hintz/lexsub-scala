package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalItem

// General note for fans of NLP frameworks:
// PLEASE don't try to use "universal" types for NLP stuff
// Instead, use types tailored to YOUR application. This way, we get type safety on the level where we need it
// (Types are there to help you, not get in your way)

/** A pre-processed token within a sentence */
case class Token(val word: String, val pos: String, val lemma: String)

/** A pre-processed sentence */
case class Sentence(val tokens: Vector[Token]) // val sent: String,

/** A pre-processed item */
case class LexSubInstance(val sentence: Sentence, val headIndex: Int, gold: Option[GermEvalItem]){
  def head: Token = sentence.tokens(headIndex)
}

/** A pair of (lexsubInstance, substitution) with optional gold scores */
case class SubstitutionItem(
    val lexSubInstance: LexSubInstance,
    val substitution: String)
{
  def target: Token = lexSubInstance.head
  def targetLemma: String = target.lemma
  
  lazy val goldCounts: Option[(Int, Int)] = lexSubInstance.gold.map { gold => 
    val substitutions = gold.gold.substitutions
    val total = substitutions.map(_._2).sum
    substitutions.collectFirst { case (`substitution`, c) => (c, total)}.getOrElse((0, total))
  }
  
  def isGood = goldCounts.map(_._1 > 0)
  def score = goldCounts.map { case (a, b) => a.toDouble / b }
}