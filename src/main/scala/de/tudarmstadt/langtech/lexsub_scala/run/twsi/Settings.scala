package de.tudarmstadt.langtech.lexsub_scala.run.twsi

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.reader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features.{ Cooc, EditDistance, DTLookup, SalientDTFeatures, BinaryWordSimilarity, PosContextWindows, PairFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity, Word2VecLookup }
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing

/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("twsi-paths.yaml") {

  val twsiFolder = path("twsiFolder")

  // folder to store training data and trained model
  val trainingDir = path("trainingFolder")

  // output file
  val instancesOutputFile = path("outputFile")

  // intermediate files
  val targetsFile = "resources/twsi/targets.txt"
  val targetsPosFile = "resources/twsi/targets-pos.txt"
  val vocabFile = "resources/twsi/vocab.txt"

  // Defines complete processing
  implicit lazy val preprocessing = SimpleProcessing(
    tokenize = new SimpleProcessing.Tokenizer {
      lazy val model = new TokenizerME(new TokenizerModel(new File((path("Preprocessing", "opennlpTokenModel")))))
      def apply(sent: String) = model.tokenize(sent)
    },

    posTag = new SimpleProcessing.PosTagger {
      lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "opennlpPOSModel"))))
      def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
    },

    lemmatize = identity // no need
    )

  val semevalData = io.lazySerialized("cache/twsi_data.ser") {
    new SemEvalReader(twsiFolder, "twsi2_clean.xml", "twsi2.gold").items
  }

  // parse the data
  lazy val lexsubData = io.lazySerialized("cache/twsi_parsed.ser") {
    preprocessing.parseSemEval(semevalData)
  }


  // Candidate lists
  object candidates {
    lazy val wordnet = new CandidateFile(path("Candidates", "wordnet"), true)
    lazy val masterlist = new CandidateFile(path("Candidates", "masterlist"), true)
    lazy val gold = new CandidateFile(path("Candidates", "gold"), true)

    // shortcut to select candidate lists
    lazy val trainingList = gold
    lazy val systemList = wordnet
  }

  // N-gram counts
  object ngrams {
    lazy val web1t = Web1TLookup(path("NGrams", "web1t"), 5)
  }

  // Word embeddings
  object embeddings {
    val word2vecPruning = Integer.MAX_VALUE
    //lazy val word2vec = Word2VecLookup(path("Embeddings", "word2vec"), word2vecPruning)
    //lazy val eigenword = WordVectorFileLookup(path("Embeddings", "eigenwords"))
  }

  // DTs
  object dts {
    
    lazy val firstOrder = DTLookup("LMI_1stOrder", new WordSimilarityFile(path("DT", "firstOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT
     
      
    lazy val secondOrder = DTLookup("LMI_2ndOrder", new WordSimilarityFile(path("DT", "secondOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT
      
      
   lazy val smallFirstOrder = DTLookup("LMI_small1stOrder", new WordSimilarityFile(path("DT", "smallFirstOrder"), identity, matchPrefix = true),
      token => token.lemma.toLowerCase + "#" + token.pos.take(1).toUpperCase, // how to look up token in this DT
      (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT

  }

  // Co-occurence features
 lazy val cooc = DTLookup("cooc", 
     new WordSimilarityFile(path("Coocs", "oldCoocs"), _.takeWhile(_ != '/')), 
     lookupFunction = token => token.word)

  lazy val ngramCounts = ngrams.web1t

  // setup features
  lazy val features = new Features(
    ///SentenceIDFeature,
    ///SubstitutionFeature,
    
    Cooc(cooc),
    
    // to what extend the context characterizes the subst
    SalientDTFeatures(dts.smallFirstOrder),
    
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
    ConjunctionFreqRatio(ngramCounts, Seq("and", "or", ","), 0, 0),
    
    // lexical resource features
    NumLexSemRelations(candidates.systemList),
    LexSemRelation(candidates.systemList) 
    )
  
  // Others:
  // WordEmbeddingDistanceVectorsSet(embeddings.word2vec, 0 to 2, 0 to 2, 5),
  // WordEmbeddingSimilarity(embeddings.word2vec)
  // WordEmbeddingDistance(embeddings.word2vec)
  // EditDistance
}
