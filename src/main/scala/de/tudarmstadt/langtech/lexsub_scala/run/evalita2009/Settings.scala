package de.tudarmstadt.langtech.lexsub_scala.run.evalita2009

import de.tudarmstadt.langtech.scala_utilities.formatting.de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil


/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("evalita2009-paths.yaml") {
  
  val evalitaFolder = path("evalitaFolder")
  
    // Defines complete processing
  implicit lazy val preprocessing = SimpleProcessing(

      tokenize = (s: String) => "[àèéìòóùÀÈÉÌÒÓÙ\\w]+".r.findAllIn(s).toVector, // hopefully I got all here
      posTag = tokens => tokens.map(x => "?"),
      lemmatize = identity // no need
   )
  
  // load the evalita data (from cache, if available)
  lazy val evalitaTest = LexsubUtil.preprocessSemEval(evalitaFolder, "test/lexsub_test.xml", "test/gold.test")
  lazy val evalitaTrial = LexsubUtil.preprocessSemEval(evalitaFolder, "trial/lexsub_trial.xml", "trial/gold.trial")
  
}