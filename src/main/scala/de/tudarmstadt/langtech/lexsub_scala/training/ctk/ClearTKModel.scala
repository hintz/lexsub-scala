package de.tudarmstadt.langtech.lexsub_scala.training.ctk

import java.io.File
import scala.collection.JavaConversions.seqAsJavaList
import org.cleartk.classifier.Feature
import org.cleartk.classifier.Instance
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities.collections
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.processing.BatchProcessing
import de.tudarmstadt.langtech.lexsub_scala.training.Featurizer
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer

/**
 * A ClearTK model building a classifier for pointwise ranking
 * @param classifier classifier to be used by CTK (default: "MaxEnt")
 */
class ClearTKModel(val classifier: String = "MaxEnt") extends Model {

  def train(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])], trainingFolder: String){
    val instances = new CTKInstanceBuilder(useScores = false)(featurizedData)
    trainAndPackage(instances, trainingFolder)
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

  /** Performs crossvalidate, prints results to stdout and writes aggregated results to outputFile */
  @Deprecated
  def crossvalidate(data: Iterable[LexSubInstance],
                    trainingList: CandidateList, systemList: CandidateList, features: Features,
                    trainingRoot: String, outputFile: String, folds: Int = 10, maxItems: Int = 20) {

    println("Starting crossvalidation on " + data.size + " instances")

    val trainingData = Model.createTrainingData(data, trainingList)
    println("Using %d instances with candidates from %s created %d training examples".format(data.size, trainingList, trainingData.map(_.candidates.size).sum))

    println("Extracting features..")
    val featurizedData = Featurizer(features)(trainingData)
    
    val instanceMaker = new CTKInstanceBuilder(useScores = false)
    val instances = instanceMaker(featurizedData)

    val dataWithFeatures = trainingData.zip(instances)

    println("Grouping data and creating folds..")
    val grouping = (instance: LexSubInstance) => instance.gold.get.sentence.target.lemma
    val grouped = dataWithFeatures.groupBy(x => grouping(x._1.lexSubInstance))
    val folded = collections.crossfold(grouped.keys.toSeq, folds)

    val outcomes = for (((heldOutItems, trainingItems), i) <- folded.zipWithIndex) yield {
      val trainingFolder = trainingRoot + "/fold" + i
      val folder = new File(trainingFolder); folder.mkdir
      println("Fold %d (items %s)".format(i + 1, heldOutItems.mkString(", ")))
      val trainingData: Iterable[Instance[String]] = trainingItems.flatMap(grouped.apply).map(_._2)
      trainAndPackage(instances, folder)

      val testData: Seq[Substitutions] = heldOutItems.flatMap(grouped.apply).map(_._1)
      val testInstaces = testData.map(_.lexSubInstance)

      val lexsub = LexSubExpander(systemList, features, getScorer(trainingFolder), maxItems = maxItems)
      val ranked = lexsub(testInstaces)
      val results = Outcomes.collect(testInstaces, ranked)
      val oot = Outcomes.evaluate(results, 10)
      val best = Outcomes.evaluate(results, 1)
      println("Fold %d: best=%s oot=%s".format(i + 1, best, oot))
      results
    }

    SemEvalResultOutcomeWriter.save(outcomes.flatten, outputFile)
    io.write(outputFile + ".system.txt", LexSubExpander(systemList, features, null, maxItems = maxItems).toString)

    val results = outcomes.flatten
    val oot = Outcomes.evaluate(results, 10)
    val best = Outcomes.evaluate(results, 1)
    println("Overall best=[%s] oot=[%s]".format(best, oot))
  }
}

/** Creates instances for training a ClearTK classifier */
class CTKInstanceBuilder(val useScores: Boolean) {

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

  def process(item: Substitutions, features: Vector[Seq[Feature]]): Vector[Instance[String]] = {
    /*if(useScores) {
      val outcomes = item.asItems.map(_.score.get)
      val instances = features.zip(outcomes).map(mkNumericInstance.tupled)
      return instances.asInstanceOf[Vector[Instance[Any]]]
    }*/

    val gold = item.asItems.map(_.isGood.get)
    val outcomes = gold.map(if (_) CTKInstanceBuilder.Good else CTKInstanceBuilder.Bad)
    val instances = features.zip(outcomes).map(mkInstance.tupled)
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
}
