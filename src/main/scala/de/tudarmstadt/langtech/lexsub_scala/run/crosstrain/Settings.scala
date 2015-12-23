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
  
  // we can use the same scorer script for all semeval tasks. GermEval supplies the most recent
  val scorerFolder = path("Tasks", "germevalFolder") + "/germeval2015-scorer"
  
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
    
    lazy val dtFirstOrder = DTLookup("DT1st", new WordSimilarityFile(path("DT", "German", "firstOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))
    lazy val dtSecondOrder = DTLookup("DT2nd", new WordSimilarityFile(path("DT", "German", "secondOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))
    
    lazy val trainingData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "train-dataset")
    lazy val testData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "test-dataset")
    val testGoldfile = path("Tasks", "germevalFolder") + "/test-dataset.gold"
    
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
    val testGoldfile = path("Tasks", "semevalFolder") + "/trial/gold.trial"
    def trainingData = semevalTest
    def testData = semevalTrial

    lazy val globalWordnetHyHo = new CandidateFile(path("Candidates", "English", "wordnet-hy-ho"), true)
    lazy val candidates = globalWordnetHyHo
    lazy val ngrams = Web1TLookup(path("NGrams", "English", "web1t"), 5)
    val conjunctions = Seq("and", "or", ",")
    
    lazy val dtFirstOrder = DTLookup("DT1st", 
      new WordSimilarityFile(path("DT", "English", "firstOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase,
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#"))
    lazy val dtSecondOrder =  DTLookup("DT2nd", 
      new WordSimilarityFile(path("DT", "English", "secondOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase,
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#"))
    
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
    val testGoldfile = path("Tasks", "evalitaFolder") + "/trial/gold.trial"

    lazy val multiwordnet = new CandidateFile(path("Candidates", "Italian", "Evalita2009", "multiwordnet"), true)
    lazy val candidates = multiwordnet
    lazy val ngrams = Web1TLookup(path("NGrams", "Italian", "web1t"), 5)
    val conjunctions = Seq("e", "ed", "o", "od", ",")
    
    lazy val dtFirstOrder = DTLookup("DT1st", new WordSimilarityFile(path("DT", "Italian", "firstOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))
    lazy val dtSecondOrder = DTLookup("DT2nd", new WordSimilarityFile(path("DT", "Italian", "secondOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))

    val trainingFolder = "trainingItalian"
    
    lazy val features = mkFeatures(this)
  }
  

  def mkFeatures(lang: LanguageData): FeatureAnnotator = {
    new FeatureAnnotator(
      // syntactic features
      PosContextWindows(0 to 1, 0 to 1, 3),
      
      // to what extend the context characterizes the subst
      SalientDTFeatures(lang.dtFirstOrder),
      // similarity between target and subst
      /// WordSimilarity(dts.secondOrder),
      // top-k similar words
      AllThresholdedDTFeatures(
        dts = Seq(lang.dtSecondOrder),
        restrictToContext = Seq(false),
        Seq(5, 20, 50, 100, 200)),
      // top-k similar context-features, with and without restriction to sent context
      AllThresholdedDTFeatures(
        dts = Seq(lang.dtFirstOrder),
        restrictToContext = Seq(true, false),
        Seq(5, 20, 50, 100, 200)),
      // boolean feature if target/substitute are similar
      BinaryWordSimilarity(lang.dtSecondOrder, 100),
      
      // frequency features
      PairFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      SetFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(lang.ngrams, lang.conjunctions, 0, 0, false),
      // semantic relations
      NumLexSemRelations(lang.candidates)
    )
  }
}