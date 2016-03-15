package de.tudarmstadt.langtech.lexsub_scala.scorer

import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.utility.RankEntry
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibMapper
import de.tudarmstadt.langtech.lexsub_scala.features.Feature
import ciir.umass.edu.learning.RankerFactory
import ciir.umass.edu.learning.Ranker
import ciir.umass.edu.learning.SparseDataPoint
import ciir.umass.edu.learning.DataPoint
import ciir.umass.edu.learning.RankList

class RankLibScorer(val modelFolder: String) extends Scorer {
  
  lazy val featureMapping = io.deserialize[RankLibMapper](RankLibModel.getFeatureMappingFile(modelFolder))
  lazy val ranker: Ranker = (new RankerFactory).loadRanker(RankLibModel.getModelFile(modelFolder))
  //lazy val ranklibWrapper = new RankLibWrapper(RankLibModel.getModelFile(modelFolder))
  
  def apply(featureVector: Vector[Seq[Feature]]): Vector[Double] = RankLibWrapper synchronized {
    if(featureVector.isEmpty) return Vector.empty
    
    try {
      DataPoint.MAX_FEATURE = featureMapping.maxIndex + 1 // RankLib is SO ugly, this hack is needed
      
      val datapoints = featureVector.map(toDataPoint)
      val result = datapoints.map(ranker.eval)
      result
    }
    catch {
      case e: Exception =>
        System.err.println("ERROR: RankLib failed: "  + e)
        featureVector.map(_ => 0d)
    }
    
    /* Old version using system call:

    val queryId = 1000 // arbitrary
    val data = featureVector.map { features => featureMapping.toRankEntry(features, 0, queryId)}
    val output = ranklibWrapper.rank(data)
    
    if(!output.isDefinedAt(queryId)){
      System.err.println(s"WARNING: RankLib ($modelFolder) failed for item with ${featureVector.size} substitutes")
      return featureVector.map(_ => 0d)
    }
    val scores = output(queryId).toVector
    scores
    * 
    */
  }

  
  def toDataPoint(features: Seq[Feature]): DataPoint = {
    val rankEntry = featureMapping.toRankEntry(features, 0, 1000)
    val asText = RankLibWrapper.toLetorLine(rankEntry)
    val datapoint = new SparseDataPoint(asText)
    datapoint
  }
}

