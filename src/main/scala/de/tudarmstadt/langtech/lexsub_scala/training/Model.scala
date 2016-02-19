package de.tudarmstadt.langtech.lexsub_scala.training

import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.Scorer

/** A machine learning backend model. This interface just asks the backend to do training, and yield a final scorer for lexsub */
trait Model {
  
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String)
  def getScorer(trainingFolder: String): Scorer
  
}