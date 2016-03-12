package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.training.ctk.DeprecatedTraining


/** Evaluates all features (as specified in Settings) in isolation using 10-fold cv */
object RunSingleFeatureEval extends App {
  
  for(feature <- Settings.features.extractors){
    println("Crossvalidating feature " + feature + " ..")
    
    // TODO
  }
}