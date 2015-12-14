package de.tudarmstadt.langtech.lexsub_scala.run.evalita2009

import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.scala_utilities.io
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("evalita2009-paths.yaml") {

  val evalitaFolder = path("evalitaFolder")
  val resourcesFolder = path("resourcesFolder")
  val trainingFolder = path("trainingFolder")
  val instancesOutputFile = path("outputFile")

  // Defines complete processing
  implicit lazy val preprocessing = SimpleProcessing(
    tokenize = (s: String) => "[àèéìòóùÀÈÉÌÒÓÙ'\\w]+".r.findAllIn(s).toVector, // hopefully I got all here
    posTag = tokens => tokens.map(x => "?"),  // no need
    lemmatize = identity // no need
    )

  // load the evalita data (from cache, if available)
  lazy val evalitaTest = LexsubUtil.preprocessSemEval(evalitaFolder, "test/lexsub_test.xml", "test/gold.test")
  lazy val evalitaTrial = LexsubUtil.preprocessSemEval(evalitaFolder, "trial/lexsub_trial.xml", "trial/gold.trial")

  // Candidate lists
  object candidates {
    lazy val multiwordnet = new CandidateFile(path("Candidates", "multiwordnet"), true)
    lazy val systemList = multiwordnet
  }

  // N-gram counts
  object ngrams {
    lazy val web1t = Web1TLookup(path("NGrams", "web1t"), 5)
  }

  // DTs
  object dts {
    lazy val firstOrder = DTLookup("DT_1st", new WordSimilarityFile(path("DT", "itWac_1stOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT

    lazy val secondOrder = DTLookup("DT_2nd", new WordSimilarityFile(path("DT", "itWac_2ndOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT
  }

  lazy val ngramCounts = ngrams.web1t

  // setup features
  lazy val features = new FeatureAnnotator(
     
      // to what extend the context characterizes the subst
    SalientDTFeatures(dts.firstOrder),
    
    // similarity between target and subst
    /// WordSimilarity(dts.secondOrder),
    
    // top-k similar words
    AllThresholdedDTFeatures(
      dts = Seq(dts.secondOrder),
      restrictToContext = Seq(false),
      Seq(5, 20, 50, 100, 200)),
      
    // top-k similar context-features, with and without restriction to sent context
    AllThresholdedDTFeatures(
      dts = Seq(dts.firstOrder),
      restrictToContext = Seq(true, false),
      Seq(5, 20, 50, 100, 200)),
      
    // boolean feature if target/substitute are similar
    BinaryWordSimilarity(dts.secondOrder, 100),
      
    // syntactic features
    PosContextWindows(0 to 1, 0 to 1, 3),
    
    // freq features
    PairFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    SetFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    ConjunctionFreqRatio(ngramCounts, Seq("e", "ed", "o", "od", ","), 0, 0),
    
    // lexical resource features
    NumLexSemRelations(candidates.systemList),
    LexSemRelation(candidates.systemList)
    )

}