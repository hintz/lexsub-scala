package de.tudarmstadt.langtech.lexsub_scala.scorer

import java.io.File
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import org.cleartk.classifier.Classifier
import org.cleartk.classifier.Instance
import org.cleartk.classifier.Feature
import org.cleartk.classifier.jar.JarClassifierBuilder
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.scala_utilities.processing.BatchProcessing
import de.tudarmstadt.langtech.lexsub_scala.PointwiseScorer


/** A scorer based on a ClearTK classifier trained in trainingDirectory */
case class CTKScorer(val trainingDiretory: String) extends PointwiseScorer {
  import org.cleartk.classifier.mallet.MalletStringOutcomeClassifierBuilder
  import org.cleartk.classifier.Classifier
  import org.cleartk.classifier.Feature
    
  val classifier = JarClassifierBuilder
    .fromTrainingDirectory(new File(trainingDiretory))
    .loadClassifierFromTrainingDirectory(new File(trainingDiretory))
    .asInstanceOf[Classifier[String]]
  
  def apply(features: Seq[Feature]): Double = {
    val outcomes = classifier.score(features, 2)
    val goodScore = outcomes.collectFirst { case o if o.getOutcome == CTKInstanceBuilder.Good => o.getScore }
    goodScore.getOrElse(throw new IllegalStateException)
  }
}


/** Creates instances for training a ClearTK classifier */
class CTKInstanceBuilder(val features: Features) extends BatchProcessing[Substitutions, Vector[Instance[String]]] {
  val useScores = true

  private val mkInstance = (features: Seq[Feature], outcome: String) => {
    val result = new Instance[String]
    result setOutcome outcome
    result addAll features
    result
  }
  
  private val mkNumericInstance = (features: Seq[Feature], outcome: Double) => {
    val result = new Instance[Double]
    result setOutcome outcome
    result addAll features
    result
  }
  
  def apply(item: Substitutions): Vector[Instance[String]] = {
    val feats = features.extract(item)
    
    /*if(useScores) {
      val outcomes = item.asItems.map(_.score.get)
      val instances = features.zip(outcomes).map(mkNumericInstance.tupled)
      return instances.asInstanceOf[Vector[Instance[Any]]]
    }*/

    val gold = item.asItems.map(_.isGood.get)
    val outcomes = gold.map(if(_) CTKInstanceBuilder.Good else CTKInstanceBuilder.Bad)
    val instances = feats.zip(outcomes).map(mkInstance.tupled)
    instances

  }
  
  override def toString = "CTKBinaryFeatureAnnotator(%s)".format(features.extractors.mkString("\n", "\n", "\n"))
}

object CTKInstanceBuilder {
  // some arbitrary labels for good and bad instances
  val Good = "GOOD"
  val Bad = "BAD"
}
