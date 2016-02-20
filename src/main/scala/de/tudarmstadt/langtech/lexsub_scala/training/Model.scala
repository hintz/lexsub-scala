package de.tudarmstadt.langtech.lexsub_scala.training

import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions

/** A machine learning backend model. This interface just asks the backend to do training, and yield a final scorer for lexsub */
trait Model {

  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String)

  def trainOnFeaturizedData(dataWithFeatures: Iterable[Substitutions], trainingFolder: String) {

  }

  def getScorer(trainingFolder: String): Scorer

  /**
   * Pairs each LexSubInstance with a number of possible substitutes, based on a candidate list
   *  @param includeGoldNotInList val IncludeGoldNotInList (default: false)
   *         adds gold items which are not in candidate list to training.
   *         Setting this to true drastically hurts performance!
   */
  def createTrainingData(data: Iterable[LexSubInstance], candidates: CandidateList, includeGoldNotInList: Boolean = true): Iterable[Substitutions] =
    {
      data.map { instance =>
        val headLemma = instance.head.lemma
        val listReplacements = candidates.get(headLemma)
          .collect { case c if c.replacement != headLemma => c.replacement } // exclude headLemma from candidates!
          .toSet
        val replacements =
          if (!includeGoldNotInList)
            listReplacements
          else {
            val goldReplacements = instance.gold.map(g => g.gold.substitutionWords)
              .getOrElse(List.empty)
            listReplacements.union(goldReplacements.toSet)
          }
        Substitutions(instance, replacements.toVector)
      }
    }
  
  /** Creates training data from gold items */
  def createTrainingDataFromGold(data: Iterable[LexSubInstance]): Iterable[Substitutions] = {
    data.map { x => Substitutions(x, x.getGold.gold.substitutionWordsWithoutMultiwords.toVector) }
  }
}