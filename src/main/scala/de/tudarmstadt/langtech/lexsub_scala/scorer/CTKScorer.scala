package de.tudarmstadt.langtech.lexsub_scala.scorer

import java.io.File
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList
import de.tudarmstadt.langtech.lexsub_scala.PointwiseScorer
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKInstanceBuilder
import de.tudarmstadt.langtech.lexsub_scala.features.Feature


/** A scorer based on a ClearTK classifier trained in trainingDirectory */
case class CTKScorer(val trainingDiretory: String) extends PointwiseScorer {
  import org.cleartk.classifier.mallet.MalletStringOutcomeClassifierBuilder
  import org.cleartk.classifier.Classifier
  import org.cleartk.classifier.{Feature => CTKFeature}
  import org.cleartk.classifier.jar.JarClassifierBuilder
    
  val classifier = JarClassifierBuilder
    .fromTrainingDirectory(new File(trainingDiretory))
    .loadClassifierFromTrainingDirectory(new File(trainingDiretory))
    .asInstanceOf[Classifier[String]]
  
  def apply(features: Seq[Feature]): Double = {
    val ctkFeatures = CTKInstanceBuilder.translate(features)
    val outcomes = classifier.score(ctkFeatures, 2)
    val goodScore = outcomes.collectFirst { case o if o.getOutcome == CTKInstanceBuilder.Good => o.getScore }
    goodScore.getOrElse(throw new IllegalStateException)
  }
}