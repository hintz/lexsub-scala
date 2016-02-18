package de.tudarmstadt.langtech.lexsub_scala.run.misc

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.compute
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalGold
import de.tudarmstadt.langtech.lexsub_scala.reader.GoldItem
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings

object EvaluateSemevalAmbiguity extends App {
  
  
  val goldData = List(
    ("SemevalTest", Settings.English.testData),
    ("SemevalTrain", Settings.English.trainingData),
    ("GermEvalTest", Settings.German.testData),
    ("GermEvalTrain", Settings.German.trainingData),
    ("EvalitaTrain", Settings.Italian.testData),
    ("EvalitaTest", Settings.Italian.trainingData)
   )
    
  for((name, data) <- goldData){
    val gold = data.map(_.getGold.gold)
    val md = meanDice(gold)
    val byPos = meanDiceByPos(gold)
    println(s"$name: meanDice = $md%2.2f byPos=$byPos")
  }
  
  def meanDiceByPos(gold: Seq[GoldItem]) = {
    gold.groupBy(_.target.pos).mapValues(meanDice)
  }

  def meanDice(gold: Seq[GoldItem]): Double = {
    val byTarget = gold.groupBy(_.target).mapValues(_.map(_.substitutionWords.toSet))
    val meanDices = for ((target, substs) <- byTarget) yield {

      val union = substs.flatten.toSet
      val n = union.size

      val dices = for (a <- substs; b <- substs if a != b) yield {
        val intersect = a intersect b
        val dice = 2f * intersect.size / (a.size + b.size)
        //val jaccard = intersect.size.toFloat / (a union b).size
        dice
      }

      val meanDice = compute.mean(dices)
      //println(target + " -> " + n)
      dices
    }

    compute.mean(meanDices.flatten)
  }

}