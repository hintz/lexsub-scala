package de.tudarmstadt.langtech.lexsub_scala.training

import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.Scorer
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.scala_utilities.processing.BatchProcessing
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureExtractor
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander

/** A machine learning backend model. This interface just asks the backend to do training, and yield a final scorer for lexsub */
trait Model {

  /** Train from featurized data (train model only) */
  def train(featurizedData: Iterable[(Substitutions, Vector[Seq[Feature]])], trainingFolder: String)

  /** Yield a scorer given a folder with a trained model */
  def getScorer(trainingFolder: String): Scorer

  /** Train from training instances (featurize, train model) */
  def train(trainingInstances: Iterable[Substitutions], features: Features, trainingFolder: String) {
    val featurizedData = Featurizer(features)(trainingInstances)
    train(featurizedData, trainingFolder)
  }
  
  /** Train using candidate list (generate candidates, featurize, train model). Also yields resulting lexsub system */
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String): LexSubExpander = {
    val trainingInstances = Model.createTrainingData(data, candidates)
    train(trainingInstances, features, trainingFolder)
    LexSubExpander(candidates, features, getScorer(trainingFolder))
  }
}


object Model {

  /**
   * Pairs each LexSubInstance with a number of possible substitutes, based on a candidate list
   *  @param includeGoldNotInList val IncludeGoldNotInList (default: false)
   *         adds gold items which are not in candidate list to training.
   *         Setting this to true drastically hurts performance
   *  @param silent if true, no summary will be printed
   */
  def createTrainingData(data: Iterable[LexSubInstance], candidates: CandidateList,
                         includeGoldNotInList: Boolean = true, silent: Boolean = false): Iterable[Substitutions] =
    {
      val result = data.map { instance =>
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

      if (!silent) println(
        "Using %d instances with candidates from %s created %d training examples"
          .format(data.size, candidates, result.map(_.candidates.size).sum))
      result
    }

  /** Creates training data from gold items */
  def createTrainingDataFromGold(data: Iterable[LexSubInstance]): Iterable[Substitutions] = {
    data.map { x => Substitutions(x, x.getGold.gold.substitutionWordsWithoutMultiwords.toVector) }
  }
}

/** Utility class to batch-featurize data with reporting */
case class Featurizer(extractor: FeatureExtractor) extends BatchProcessing[Substitutions, (Substitutions, Vector[Seq[Feature]])] {
  def apply(item: Substitutions) = (item, extractor.extract(item))
}