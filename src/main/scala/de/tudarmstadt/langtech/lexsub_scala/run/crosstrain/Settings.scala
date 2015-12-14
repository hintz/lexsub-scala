package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import java.io.File
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.reader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import opennlp.tools.tokenize.TokenizerME
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.utility.MaltProcessing
import opennlp.tools.tokenize.TokenizerModel

object Settings extends YamlSettings("crosstraining-paths.yaml") {
  
  object German extends LanguageData {

    implicit lazy val preprocessing = SimpleProcessing(
      tokenize = (s: String) => "[äöüÄÖÜß\\w]+".r.findAllIn(s).toVector, // still works best for German
      posTag = new SimpleProcessing.PosTagger {
        lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "German", "opennlpPOSModel"))))
        def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
      },
      lemmatize = identity // no need
      )

    lazy val ngrams = Web1TLookup(path("NGrams", "German", "web1t"), 5)
    lazy val candidates = new CandidateFile(path("Candidates", "German", "GermEval2015", "masterlist"), true)
    val conjunctions = Seq("und", "oder", ",")

    lazy val trainingData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "train-dataset")
    lazy val testData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "test-dataset")
    
    val trainingFolder = "trainingGerman"
    
    lazy val features = mkFeatures(this)
  }

  object English extends LanguageData {

    lazy val tokenizer = new SimpleProcessing.Tokenizer {
      lazy val model: TokenizerME = new TokenizerME(new TokenizerModel(new File((path("Preprocessing", "English", "opennlpTokenModel")))))
      def apply(sent: String) = model.tokenize(sent)
    }

    lazy val tagger = new SimpleProcessing.PosTagger {
      lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "English", "opennlpPOSModel"))))
      def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
    }

    implicit lazy val preprocessing = SimpleProcessing(tokenizer, tagger, identity)
    
    lazy val semevalTrial = LexsubUtil.preprocessSemEval(path("Tasks", "semevalFolder"), "trial/lexsub_trial.xml", "trial/gold.trial")
    lazy val semevalTest = LexsubUtil.preprocessSemEval(path("Tasks", "semevalFolder"), "test/lexsub_test.xml", "test/gold.gold")
    def trainingData = semevalTest
    def testData = semevalTrial

    lazy val globalWordnetHyHo = new CandidateFile("resources/wordnet-hy-ho", true)
    lazy val candidates = globalWordnetHyHo
    lazy val ngrams = Web1TLookup(path("NGrams", "English", "web1t"), 5)
    val conjunctions = Seq("and", "or", ",")
    
    val trainingFolder = "trainingEnglish"
    
    lazy val features = mkFeatures(this)
  }

  object Italian extends LanguageData {

    // Defines complete processing
    implicit lazy val preprocessing = SimpleProcessing(
      tokenize = (s: String) => "[àèéìòóùÀÈÉÌÒÓÙ'\\w]+".r.findAllIn(s).toVector, // hopefully I got all here
      posTag = tokens => tokens.map(x => "?"), // no need
      lemmatize = identity // no need
     )

    // load the evalita data (from cache, if available)
    lazy val trainingData = LexsubUtil.preprocessSemEval(path("Tasks", "evalitaFolder"), "test/lexsub_test.xml", "test/gold.test")
    lazy val testData = LexsubUtil.preprocessSemEval(path("Tasks", "evalitaFolder"), "trial/lexsub_trial.xml", "trial/gold.trial")

    lazy val multiwordnet = new CandidateFile(path("Candidates", "Italian", "Evalita2009", "multiwordnet"), true)
    lazy val candidates = multiwordnet
    lazy val ngrams = Web1TLookup(path("NGrams", "Italian", "web1t"), 5)
    val conjunctions = Seq("e", "ed", "o", "od", ",")

    val trainingFolder = "italianTraining"
    
    lazy val features = mkFeatures(this)
  }
  

  def mkFeatures(lang: LanguageData): FeatureAnnotator = {
    new FeatureAnnotator(
      // syntactic features
      PosContextWindows(0 to 1, 0 to 1, 3),

      // freq features
      PairFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      SetFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(lang.ngrams, lang.conjunctions, 0, 0))
  }
}