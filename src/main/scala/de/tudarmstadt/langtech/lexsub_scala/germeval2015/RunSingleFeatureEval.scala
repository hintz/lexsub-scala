package de.tudarmstadt.langtech.lexsub_scala.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.scala_utilities.io


/** Evaluates all features (as specified in Settings) in isolation using 10-fold cv */
object RunSingleFeatureEval extends App {
  
  for(feature <- Settings.features.features){
    println("Crossvalidating feature " + feature + " ..")
    
   Training.crossvalidate(
      Settings.germevalTraining,
      Settings.candidates.trainingList,
      Settings.candidates.systemList,
      new FeatureAnnotator(feature),
      Settings.trainingDir + "/" + feature, 
      "./evalFeatures/" + feature + "/instances.out",
      folds = 10)
  }
}