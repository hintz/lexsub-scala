package de.tudarmstadt.langtech.lexsub_scala.scorer

import java.io.File

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList

import org.cleartk.classifier.Classifier
import org.cleartk.classifier.Feature
import org.cleartk.classifier.jar.JarClassifierBuilder

import de.tudarmstadt.langtech.lexsub_scala.PointwiseScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKInstanceBuilder


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