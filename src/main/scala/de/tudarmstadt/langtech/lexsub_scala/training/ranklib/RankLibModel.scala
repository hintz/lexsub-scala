package de.tudarmstadt.langtech.lexsub_scala.training.ranklib

import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.scorer.RankLibScorer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining

object RankLibModel extends Model {
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String){
    
  }
  
  def getScorer(trainingFolder: String) = new RankLibScorer(trainingFolder)
}