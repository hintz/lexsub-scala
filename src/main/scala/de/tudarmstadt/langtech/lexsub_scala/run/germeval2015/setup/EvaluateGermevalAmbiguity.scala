package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015.setup

import de.tudarmstadt.langtech.lexsub_scala.run.germeval2015.Settings
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.compute
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalGold
import de.tudarmstadt.langtech.lexsub_scala.reader.GoldItem

object EvaluateGermevalAmbiguity extends App {
  val gold = new SemEvalReader(Settings.germevalFolder, "train-dataset").gold.items
  val gold2 = new SemEvalGold("../AIPHES_Data/SemEval2007/trial/gold.trial").items
  
  println(meanDiceByPos(gold))
  println(meanDice(gold))
  println(meanDiceByPos(gold2))
  println(meanDice(gold2))
  
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