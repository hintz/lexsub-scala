package de.tudarmstadt.langtech.lexsub_scala.types

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem

/** A pre-processed token within a sentence */
case class Token(val word: String, val pos: String, val lemma: String)

/** A pre-processed sentence */
case class Sentence(val tokens: Vector[Token], val edges: List[DepEdge] = List.empty)

/** Dependency edges */
case class DepEdge(label: String, from: Int, to: Int)

/** A pre-processed item */
@SerialVersionUID(2L)
case class LexSubInstance(val sentence: Sentence, val headIndex: Int, gold: Option[SemEvalItem]) {
  def head: Token = sentence.tokens(headIndex)
  def id: String = gold.map(_.sentence.id) getOrElse noGold
  def getGold = gold getOrElse noGold
  private def noGold = throw new RuntimeException("gold data was required but not supplied: " + this)
}

/** A pair of (lexsubInstance, candidates) */
case class Substitutions(val lexSubInstance: LexSubInstance, candidates: Vector[String]) {
  lazy val asItems = candidates.map(c => SubstitutionItem(lexSubInstance, c))
}

/** A pair of (lexsubInstance, substitution) with optional gold scores */
case class SubstitutionItem(
  val lexSubInstance: LexSubInstance,
  val substitution: String) {
  def target: Token = lexSubInstance.head
  def targetLemma: String = target.lemma

  lazy val goldCounts: Option[(Int, Int)] = lexSubInstance.gold.map { gold =>
    val substitutions = gold.gold.substitutions
    val total = substitutions.map(_._2).sum
    substitutions.collectFirst { case (`substitution`, c) => (c, total) }.getOrElse((0, total))
  }

  def isGood = goldCounts.map(_._1 > 0)
  def score = goldCounts.map { case (a, b) => a.toDouble / b }
  def relevance = goldCounts.map { case (a, b) => a }
}

case class Outcome(val lexSubInstance: LexSubInstance, substitutes: Seq[(String, Double)]) {

  def toSubstituteItems: Seq[SubstitutionItem] = substitutes.map(ws => SubstitutionItem(lexSubInstance, ws._1))
  def bestOutOf(n: Int) = {
    val bestN = toSubstituteItems.take(n)
    val tp = bestN.count(_.isGood.get)
    val fp = bestN.count(!_.isGood.get)
    val numGold = lexSubInstance.gold.get.gold.substitutionWords.length
    val fn = math.min(n, numGold) - tp
    PRResult(tp, fp, fn, None)
  }

}

object Outcomes {
  import de.tudarmstadt.langtech.scala_utilities.compute

  def collect(items: Iterable[LexSubInstance], outcomes: Iterable[Seq[(String, Double)]]) =
    items.zip(outcomes).map(Outcome.tupled)

  def evaluate(outcomes: Iterable[Outcome], bestOutOf: Int = 1): PRResult = {
    outcomes.map(_.bestOutOf(bestOutOf)).reduce(_ + _)
  }

  def meanGAP(outcomes: Iterable[Outcome]): Double = {
    def gap(outcome: Outcome): Double = {
      // compute the best possible ranking scores
      val gapNormalization = {
        val goldCounts = outcome.lexSubInstance.getGold.gold.substitutions.map(_._2.toDouble)
        val sortedGoldWeights = compute.normalize(goldCounts).sorted.reverse
        val bestRankings = sortedGoldWeights.scanLeft(0d)(_ + _).tail.zipWithIndex.map {
          case (weightSum, index) => weightSum / (index + 1)
        }
        bestRankings.sum
      }
      if (gapNormalization <= 0) return 0d
      val items = outcome.toSubstituteItems
      val scores = items.map(_.score.get)
      val aggregatedScores = scores.scan(0d)(_ + _).tail
      val isGood = items.map(_.isGood.get)
      val averagePrecisions = isGood.zip(aggregatedScores.zipWithIndex).collect { case (true, (weightSum, index)) => weightSum / (index + 1) }
      averagePrecisions.sum / gapNormalization
    }
    val gaps = outcomes.map(gap)
    compute.mean(gaps)
  }
}
