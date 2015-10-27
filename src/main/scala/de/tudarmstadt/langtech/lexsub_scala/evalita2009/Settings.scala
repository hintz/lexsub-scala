package de.tudarmstadt.langtech.lexsub_scala.evalita2009

import de.tudarmstadt.langtech.scala_utilities.formatting.de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.types.Preprocessing


/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("evalita2009-paths.yaml") {
  
  val evalitaFolder = path("evalitaFolder")
  
    // Defines complete processing
  lazy val preprocessing = Preprocessing(

      tokenizer = (s: String) => "[àèéìòóùÀÈÉÌÒÓÙ\\w]+".r.findAllIn(s).toVector, // hopefully I got all here
      posTagger = tokens => tokens.map(x => "?"),
      lemmatizer = identity // no need
   )
  
  // load the evalita data (from cache, if available)
  lazy val evalitaTest = preprocessing.preprocessSemEval(evalitaFolder, "test/lexsub_test.xml", "test/gold.test")
  lazy val evalitaTrial = preprocessing.preprocessSemEval(evalitaFolder, "trial/lexsub_trial.xml", "trial/gold.trial")
  
}