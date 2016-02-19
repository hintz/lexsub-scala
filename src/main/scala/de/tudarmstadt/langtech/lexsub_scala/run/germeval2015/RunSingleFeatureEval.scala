package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.CTKTraining
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.features.Features


/** Evaluates all features (as specified in Settings) in isolation using 10-fold cv */
object RunSingleFeatureEval extends App {
  
  for(feature <- Settings.features.extractors){
    println("Crossvalidating feature " + feature + " ..")
    
   CTKTraining.crossvalidate(
      Settings.germevalTraining,
      Settings.candidates.trainingList,
      Settings.candidates.systemList,
      new Features(feature),
      Settings.trainingDir + "/" + feature, 
      "./evalFeatures/" + feature + "/instances.out",
      folds = 10)
  }
}