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
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import opennlp.tools.tokenize.TokenizerME
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing
import de.tudarmstadt.langtech.lexsub_scala.utility.MaltProcessing
import opennlp.tools.tokenize.TokenizerModel
import de.tudarmstadt.langtech.lexsub_scala.utility.MateProcessing
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator._

object Settings extends YamlSettings("crosstraining-paths.yaml") {
  
  // we can use the same scorer script for all semeval tasks. GermEval supplies the most recent
  val scorerFolder = path("Tasks", "germevalFolder") + "/germeval2015-scorer"
  
  // model location for all language data
  val allLanguagesFolder = "trainingAllLanguages"
  
  object German extends LanguageData {

    implicit lazy val preprocessing: LexSubProcessing = MateProcessing(
      tokenizer = (s: String) => "[äöüÄÖÜß\\w]+".r.findAllIn(s).toVector,
      lemmatizerModel = Some("resources/models/mate/lemma-ger-3.6.model"),
      // these are some custom trained models on universal deps:
      taggerModel = Some("resources/models/mate/tagger-de-3.6.model"),
      parserModel = Some("resources/models/mate/parser-de-3.6.model")
      )

    lazy val ngrams = Web1TLookup(path("NGrams", "German", "web1t"), 5)
    lazy val candidates = new CandidateFile(path("Candidates", "German", "GermEval2015", "masterlist"), true).filter(!_.replacement.contains("_"))
    lazy val goldCandidates = new CandidateFile(path("Candidates", "German", "GermEval2015", "gold"))
    val conjunctions = Seq("und", "oder", ",")
    
    lazy val dtFirstOrder = DTLookup("DT1st", new WordSimilarityFile(path("DT", "German", "firstOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))
    lazy val dtSecondOrder = DTLookup("DT2nd", new WordSimilarityFile(path("DT", "German", "secondOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma))
        
    lazy val coocs = DTLookup("cooc", new WordSimilarityFile(path("Coocs", "German", "germeval2015"), identity), token => token.word, cacheResults = true)
    
    lazy val w2vEmbeddings = Word2VecLookup(path("Embeddings", "German", "deNews70M"), Integer.MAX_VALUE, isLowercased = false)
    lazy val wordEmbeddings = WordVectorFileLookup(path("Embeddings", "German", "syntaxWords"), isLowercased = false)
    lazy val contextEmbeddings = WordVectorFileLookup(path("Embeddings", "German", "syntaxContexts"), isLowercased = false)
    
    lazy val trainingData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "train-dataset")
    lazy val testData = LexsubUtil.preprocessSemEval(path("Tasks", "germevalFolder"), "test-dataset")
    val testGoldfile = path("Tasks", "germevalFolder") + "/test-dataset.gold"
    val trainGoldfile = path("Tasks", "germevalFolder") + "/train-dataset.gold"
    val cvGoldfile = path("Tasks", "germevalFolder") + "/cv.gold"
    
    val trainingFolder = "trainingGerman"
    
    lazy val features = mkFeatures(this)
  }

  object English extends LanguageData {

    lazy val tagger = new SimpleProcessing.PosTagger {
      lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "English", "opennlpPOSModel"))))
      def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
    }

    implicit lazy val preprocessing: LexSubProcessing = MaltProcessing(
      tokenizer = _.split(" ").filter(_.nonEmpty).toVector, // SemEval data is pretokenized by space,
      tagger = tagger,
      lemmatizer = identity,
      maltModel = "resources/models/malt/engmalt.poly-1.7.mco")
    
    /*
    implicit lazy val preprocessing: LexSubProcessing = MateProcessing(
      tokenizer = _.split(" ").filter(_.nonEmpty).toVector, // SemEval data is pretokenized by space,
      taggerModel = Some("resources/models/mate/tagger-eng-4M-v36.mdl"),
      lemmatizerModel = Some("resources/models/mate/lemmatizer-eng-4M-v36.mdl"),
      parserModel = Some("resources/models/mate/parser-eng-12M-v36.mdl")
      )*/
    
    lazy val semevalTrial = LexsubUtil.preprocessSemEval(path("Tasks", "semevalFolder"), "trial/lexsub_trial.xml", "trial/gold.trial")
    lazy val semevalTest = LexsubUtil.preprocessSemEval(path("Tasks", "semevalFolder"), "test/lexsub_test.xml", "test/gold.gold")
    val testGoldfile = path("Tasks", "semevalFolder") + "/trial/gold.trial"
    val trainGoldfile = path("Tasks", "semevalFolder") + "/test/gold.gold"
    val cvGoldfile = path("Tasks", "semevalFolder") + "/cv.gold"
    def trainingData = semevalTest
    def testData = semevalTrial

    lazy val candidates = new CandidateFile(path("Candidates", "English", "SemEval2007", "wordnet"), true).filter(!_.replacement.contains("_"))
    lazy val goldCandidates = new CandidateFile(path("Candidates", "English", "SemEval2007", "gold"), true)
    lazy val ngrams = Web1TLookup(path("NGrams", "English", "web1t"), 5)
    val conjunctions = Seq("and", "or", ",")
    
    lazy val dtFirstOrder = DTLookup("DT1st", 
      new WordSimilarityFile(path("DT", "English", "firstOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase,
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#"))
    lazy val dtSecondOrder =  DTLookup("DT2nd", 
      new WordSimilarityFile(path("DT", "English", "secondOrder"), identity, matchPrefix = true ),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase,
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#"))
      
    lazy val coocs = DTLookup("cooc", new WordSimilarityFile(path("Coocs", "English", "semeval2007"), identity), token => token.word, cacheResults = true)
    
    lazy val wordEmbeddings = WordVectorFileLookup(path("Embeddings", "English", "syntaxWords"), isLowercased = true)
    lazy val contextEmbeddings = WordVectorFileLookup(path("Embeddings", "English", "syntaxContexts"), isLowercased = true)
    lazy val w2vEmbeddings = wordEmbeddings
    
    val trainingFolder = "trainingEnglish"
    
    lazy val features = mkFeatures(this)
  }

  object Italian extends LanguageData {

    // Defines complete processing
    implicit lazy val preprocessing: LexSubProcessing = MateProcessing(
      tokenizer = (s: String) => "[àèéìòóùÀÈÉÌÒÓÙ'\\w]+".r.findAllIn(s).toVector,
      taggerModel = Some("resources/models/mate/tagger-it-3.6.model"),
      lemmatizerModel = Some("resources/models/mate/lemmatizer-it-3.6.model"),
      parserModel = Some("resources/models/mate/parser-it-3.6.model")
      )

    // load the evalita data (from cache, if available)
    lazy val trainingData = LexsubUtil.preprocessSemEval(path("Tasks", "evalitaFolder"), "test/lexsub_test.xml", "test/gold.test")
    lazy val testData = LexsubUtil.preprocessSemEval(path("Tasks", "evalitaFolder"), "trial/lexsub_trial.xml", "trial/gold.trial")
    val testGoldfile = path("Tasks", "evalitaFolder") + "/trial/gold.trial"
    val trainGoldfile = path("Tasks", "evalitaFolder") + "/test/gold.test"
    val cvGoldfile = path("Tasks", "evalitaFolder") + "/cv.gold"

    lazy val candidates = new CandidateFile(path("Candidates", "Italian", "Evalita2009", "multiwordnet"), true).filter(!_.replacement.contains("_"))
    lazy val goldCandidates = new CandidateFile(path("Candidates", "Italian", "Evalita2009", "gold"), true)
    lazy val ngrams = Web1TLookup(path("NGrams", "Italian", "web1t"), 5)
    val conjunctions = Seq("e", "ed", "o", "od", ",")
    
    lazy val dtFirstOrder = DTLookup("DT1st", new WordSimilarityFile(path("DT", "Italian", "firstOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.contains(substitute.lemma))
    lazy val dtSecondOrder = DTLookup("DT2nd", new WordSimilarityFile(path("DT", "Italian", "secondOrder"), identity), 
        token => token.lemma, (substitute, dtFeature) => dtFeature.contains(substitute.lemma))
        
        
    lazy val coocs = DTLookup("cooc", new WordSimilarityFile(path("Coocs", "Italian", "evalita2009"), identity), token => token.word, cacheResults = true)
    
    lazy val w2vEmbeddings = Word2VecLookup(path("Embeddings", "Italian", "itWac"), Integer.MAX_VALUE, isLowercased = true) // TODO: check if these embeddings are really lowercase
    lazy val wordEmbeddings = WordVectorFileLookup(path("Embeddings", "Italian", "syntaxWords"), isLowercased = true)
    lazy val contextEmbeddings = WordVectorFileLookup(path("Embeddings", "Italian", "syntaxContexts"), isLowercased = true)


    val trainingFolder = "trainingItalian"
    
    lazy val features = mkFeatures(this)
  }
  
  
  def mkFeatures(lang: LanguageData): Features = {
    new Features(
        
      // supply source language as constant feature
      ConstantFeature("SourceLanguage", lang.toString),
      
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
      
      // co-occurence features
      Cooc(lang.coocs),
     
      // embedding n-grams
      WordEmbeddingDistanceVectorsSet(lang.w2vEmbeddings, 0 to 2, 0 to 2, 5),
        
      // Melamud's features
      SyntaxEmbeddingFeatures(lang.wordEmbeddings, lang.contextEmbeddings, Add, Mult, BalAdd, BalMult),

      // frequency features
      PairFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      SetFreqRatios(lang.ngrams, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(lang.ngrams, lang.conjunctions, 0, 0, false),
      
      // semantic relations
      NumLexSemRelations(lang.candidates),
      LexSemRelation(lang.candidates, simplifyLabels = true)

    )
  }
}