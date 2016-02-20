package de.tudarmstadt.langtech.lexsub_scala.scorer

import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.utility.RankEntry
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibMapper
import de.tudarmstadt.langtech.lexsub_scala.features.Feature

class RankLibScorer(val modelFolder: String) extends Scorer {
  
  val featureMapping = io.deserialize[RankLibMapper](RankLibModel.getFeatureMappingFile(modelFolder))
  val ranker = new RankLibWrapper(RankLibModel.getModelFile(modelFolder))
  
  def apply(featureVector: Vector[Seq[Feature]]): Vector[Double] = {
    val queryId = 1000 // arbitrary
    val data = featureVector.map { features => featureMapping.toRankEntry(features, 0, queryId)}
    val output = ranker.rank(data)
    
    if(!output.isDefinedAt(queryId)){
      System.err.println(s"WARNING: RankLib ($modelFolder) failed for item with ${featureVector.size} substitutes")
      return featureVector.map(_ => 0d)
    }
    val scores = output(queryId).toVector
    scores
  }
}