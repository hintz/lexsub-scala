package de.tudarmstadt.langtech.lexsub_scala.twsi.setup

import scala.util.Random
import de.tudarmstadt.langtech.lexsub_scala.twsi.Settings
import de.tudarmstadt.langtech.lexsub_scala.training.Training

object CreateHoldOutSplit {
  
  val random = new Random(12345)
  val holdOutPercent = 0.2 // hold out 0.2 as held out data
  val samplePercent = 0.2 // sample 0.2 from the training data
  
  val byTarget = Settings.semevalData.groupBy { x => x.gold.targetWord }
  val (trainingTargets, heldOutTargets) = Training.holdOut(random.shuffle(byTarget.keys).toSeq, holdOutPercent)
  val (_, sampleTargets) = Training.holdOut(trainingTargets, samplePercent) 
  
  val trainingIds = trainingTargets.flatMap(byTarget).map(_.gold.id).distinct
  val heldOutIds = heldOutTargets.flatMap(byTarget).map(_.gold.id).distinct
  
	println("Training targets: " + trainingTargets.mkString(" ;; "))
	println("Training ids: " + trainingIds.mkString(" ;; "))
  println("Sample targets: " + sampleTargets.mkString(" ;; "))
	println("Held out targets: " + heldOutTargets.mkString(" ;; "))
  println("Held out ids: " + heldOutIds.mkString(" ;; "))
 
 
}