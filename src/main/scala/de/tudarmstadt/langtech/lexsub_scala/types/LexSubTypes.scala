package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem

/** A pre-processed token within a sentence */
case class Token(val word: String, val pos: String, val lemma: String)

/** A pre-processed sentence */
case class Sentence(val tokens: Vector[Token]) // val sent: String,

/** Dependency edges */
case class DepEdge(label: String, from: Int, to: Int)

/** A pre-processed item */
@SerialVersionUID(1L)
case class LexSubInstance(val sentence: Sentence, val headIndex: Int, gold: Option[SemEvalItem]){
  def head: Token = sentence.tokens(headIndex)
  def id: String = gold.map(_.sentence.id) getOrElse noGold
  def getGold = gold getOrElse noGold
  private def noGold = throw new RuntimeException("gold data was required but not supplied: " + this)
}

/** A pair of (lexsubInstance, candidates) */
case class Substitutions(val lexSubInstance: LexSubInstance, candidates: Vector[String]){
  lazy val asItems = candidates.map(c => SubstitutionItem(lexSubInstance, c))
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

case class Outcome(val lexSubInstance: LexSubInstance, substitutes: Seq[(String, Double)]){
  def toSubstituteItems: Seq[SubstitutionItem] = substitutes.map(ws => SubstitutionItem(lexSubInstance, ws._1))
  def bestOutOf(n: Int) = {
    val bestN = toSubstituteItems.take(n)
    val tp = bestN.count(_.isGood.get)
    val fp = bestN.count(! _.isGood.get)
    val numGold = lexSubInstance.gold.get.gold.substitutionWords.length
    val fn = math.min(n, numGold) - tp
    PRResult(tp, fp, fn, None)
  }
}

object Outcomes {
  def collect(items: Iterable[LexSubInstance], outcomes: Iterable[Seq[(String, Double)]]) = 
    items.zip(outcomes).map(Outcome.tupled)
    
  def evaluate(outcomes: Iterable[Outcome], bestOutOf: Int = 1): PRResult = {
    outcomes.map(_.bestOutOf(bestOutOf)).reduce(_ + _)
  }
}
