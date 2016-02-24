package de.tudarmstadt.langtech.lexsub_scala.training.ctk

import java.io.File
import scala.collection.JavaConversions.seqAsJavaList
import org.cleartk.classifier.{Feature => CTKFeature}
import org.cleartk.classifier.Instance
import org.cleartk.classifier.jar.JarClassifierBuilder
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Feature
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.Featurizer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities.collections
import de.tudarmstadt.langtech.scala_utilities.io
import scala.concurrent.Future

/**
 * A ClearTK model building a classifier for pointwise ranking
 * @param classifier classifier to be used by CTK (default: "MaxEnt")
 */
class ClearTKModel(val classifier: String = "MaxEnt") extends Model {

  def train(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])], trainingFolder: String): Future[Unit] = {
    val instances = new CTKInstanceBuilder(useScores = false)(featurizedData)
    trainAndPackage(instances, trainingFolder)
    Future.successful(()) // no multithreading for now
  }

  def getScorer(trainingFolder: String) = CTKScorer(trainingFolder)

  /** Calls the training algorithm of CTK */
  def trainAndPackage(instances: Iterable[Instance[String]], trainingDir: File) {

    println("Training on " + instances.size + " instances.. Writing training data to " + trainingDir)
    val dataWriter = new MalletStringOutcomeDataWriter(trainingDir)
    instances foreach dataWriter.write
    dataWriter.finish

    //val writer = new InstanceDataWriter[String](trainingDir)
    //dataWriter.getClassifierBuilder.trainClassifier(trainingDir, "MaxEnt")
    //dataWriter.getClassifierBuilder.packageClassifier(trainingDir)

    println("Starting train & package..")
    JarClassifierBuilder.trainAndPackage(trainingDir, classifier)
  }

  /** Calls the training algorithm of CTK */
  def trainAndPackage(instances: Iterable[Instance[String]], trainingFolder: String) {
    trainAndPackage(instances, new java.io.File(trainingFolder))
  }

  def writeInstances(filename: String, items: Iterable[Substitutions]) {
    val instances = items.flatMap(_.asItems)
    val lines = for ((substItem @ SubstitutionItem(instance, substitution)) <- instances) yield {
      val line = Seq(instance.gold.get.gold.id, substitution, substItem.score.get)
      line.mkString("\t")
    }
    io.write(filename, lines.mkString("\n"))
  }
}

/** (deprecated) Legacy interface for ClearTK crossvalidation */
@Deprecated
object DeprecatedTraining extends ClearTKModel() {

}

/** Creates instances for training a ClearTK classifier */
class CTKInstanceBuilder(val useScores: Boolean) {

  private val mkInstance = (features: Seq[CTKFeature], outcome: String) => {
    val result = new Instance[String]
    result setOutcome outcome
    result addAll features
    result
  }

  private val mkNumericInstance = (features: Seq[CTKFeature], outcome: Double) => {
    val result = new Instance[Double]
    result setOutcome outcome
    result addAll features
    result
  }

  def process(item: Substitutions, features: Vector[Seq[Feature]]): Vector[Instance[String]] = {
    /*if(useScores) {
      val outcomes = item.asItems.map(_.score.get)
      val instances = features.zip(outcomes).map(mkNumericInstance.tupled)
      return instances.asInstanceOf[Vector[Instance[Any]]]
    }*/

    val gold = item.asItems.map(_.isGood.get)
    val outcomes = gold.map(if (_) CTKInstanceBuilder.Good else CTKInstanceBuilder.Bad)
    val translatedFeatures = features.map(CTKInstanceBuilder.translate)
    val instances = translatedFeatures.zip(outcomes).map(mkInstance.tupled)
    instances
  }
  
  def apply(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])]): Iterable[Instance[String]] = {
    featurizedData.map((process _).tupled).flatten
  }

  override def toString = s"CTKBinaryFeatureAnnotator(useScores=$useScores)"
}

object CTKInstanceBuilder {
  // some arbitrary labels for good and bad instances
  val Good = "GOOD"
  val Bad = "BAD"
  
  def asCTKFeature(feature: Feature): CTKFeature = {
    new org.cleartk.classifier.Feature(feature.name, feature.value)
  }
  
  /** translates a Seq[Feature] to a collection of org.cleartk.classifier.Feature */
  def translate(features: Seq[Feature]): Seq[CTKFeature] = features.map(asCTKFeature)
}
