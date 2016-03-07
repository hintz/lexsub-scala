package de.tudarmstadt.langtech.lexsub_scala.training.ranklib

import scala.collection.mutable.ListBuffer
import de.tudarmstadt.langtech.lexsub_scala.features.Feature
import de.tudarmstadt.langtech.lexsub_scala.scorer.RankLibScorer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.lexsub_scala.utility.RankEntry
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib.Config
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.features.NumericFeature
import scala.concurrent.Future

case class RankLibModel(rankLibConfig: RankLib.Config) extends Model {

  def train(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])], trainingFolder: String): Future[Int] = {
    // determine final files for model
    val featureMappingFile = RankLibModel.getFeatureMappingFile(trainingFolder)
    val modelFile = RankLibModel.getModelFile(trainingFolder)
    val trainingFile = RankLibModel.getTrainingFile(trainingFolder)

    // build feature mapping
    val featureMapping = RankLibMapper.build(featurizedData)

    val rankLib = new RankLibWrapper(modelFile)
    val trainingInstances = featureMapping.createTrainingData(featurizedData)

    println(s"Training RankLib model: writing to $modelFile, serializing feature mapping to $featureMappingFile")
    io.serialize(featureMapping, featureMappingFile)
    rankLib.retrain(trainingInstances, rankLibConfig, trainingFile)
  }

  def getScorer(trainingFolder: String) = new RankLibScorer(trainingFolder)

}

object RankLibModel {
  def getFeatureMappingFile(trainingFolder: String): String = trainingFolder + "/mapping.ser"
  def getModelFile(trainingFolder: String): String = trainingFolder + "/model.txt"
  def getTrainingFile(trainingFolder: String): String = trainingFolder + "/training.txt"
}

/** Utility class for mapping sparse features to RankLib-internal dense reperesentation */
class RankLibMapper(featureMapping: Map[String, Int], val maxIndex: Int) extends Serializable {

  def toDenseFeatureVector(features: Seq[Feature]): List[(Int, Double)] = {

    val errorList = new ListBuffer[String]
    def translate(feature: NumericFeature): Option[(Int, Double)] = {
      val featureId = featureMapping.get(feature.name)
      if (featureId.isEmpty) errorList.append(feature.name)
      featureId.map { (_, feature.value) }
    }
    val tmpLookupMap = features.map(_.asNumeric).flatMap(translate).toMap
    val denseFeatureList = for (i <- 1 to maxIndex) yield {
      (i, tmpLookupMap.getOrElse(i, 0d))
    }
    
    //if(errorList.nonEmpty)
    //  System.err.println(s"WARNING: Saw ${errorList.length} unknown features: ${errorList.mkString(", ")}")

    denseFeatureList.toList
  }

  def toRankEntry(features: Seq[Feature], relevanceScore: Int, queryId: Int) =
    RankEntry(queryId, "noid", relevanceScore, toDenseFeatureVector(features))

  def createTrainingData(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])]): Iterable[RankEntry] = {
    featurizedData.zipWithIndex.flatMap {
      case ((substitutions, features), queryId) =>
        val items = substitutions.asItems
        val relevances = items.map(_.relevance.get)
        val featuresWithScores = features.zip(relevances)
        val data = featuresWithScores.map { case (features, relevanceScore) => toRankEntry(features, relevanceScore, queryId) }
        data
    }
  }

}

object RankLibMapper {
  def build(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])]): RankLibMapper = {
    val allFeatures = featurizedData.flatMap(_._2.flatten)
    val uniqueFeatureNames = allFeatures.map(_.name).toSet
    val maxFeatureId = uniqueFeatureNames.size
    val mapping = uniqueFeatureNames.zipWithIndex.map { case (fName, i) => (fName, i + 1) }
    new RankLibMapper(mapping.toMap, maxFeatureId)
  }
}
