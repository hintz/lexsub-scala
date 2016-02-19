package de.tudarmstadt.langtech.lexsub_scala.scorer

import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.utility.RankEntry

class RankLibScorer(val trainingDirectory: String) extends Scorer {
  
  val FeatureMappingFile = trainingDirectory + "/mapping.ser"
  val ModelFile = trainingDirectory + "/model.txt"
  
  val featureMapping = io.deserialize[Map[String, Int]](FeatureMappingFile)
  val ranker = new RankLibWrapper(ModelFile)
  
  def mapFeature(feature: Feature): List[(Int, Double)] = {
    val featureId = featureMapping.get(feature.getName)
    if(featureId.isEmpty)
      System.err.println("WARNING: Unknown feature " + feature.getName + " (did not see during training)")
    featureId.toList.map { (_, feature.getValue.asInstanceOf[Double])}
  }
  
  def apply(featureVector: Vector[Seq[Feature]]): Vector[Double] = {
    val queryId = 1000 // arbitrary
    val translatedFeatures = featureVector.toList.map { feats => feats.toList.flatMap(mapFeature) }
    val data = translatedFeatures.zipWithIndex.map { case (features, idx) => 
      RankEntry(queryId, idx, 0, features)
    }
    val output = ranker.rank(data)
    val scores = output(queryId).toVector
    scores
  }
}