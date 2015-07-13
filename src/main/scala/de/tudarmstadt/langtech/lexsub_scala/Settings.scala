package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.reader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features.{Cooc, DTLookup, SalientDTFeatures, BinaryWordSimilarity, PosContextWindows, PairFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity}
import de.tudarmstadt.langtech.lexsub_scala.types.Preprocessing
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.scala_utilities.formatting.de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates

/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("paths.yaml") {
  
  val germevalFolder = path("germEvalFolder")
  
  // folder to store training data and trained model
  val trainingDir = path("trainingFolder")
  
  // output file
  val instancesOutputFile = path("outputFile")
  
  // GermaNet location (only needed to create candidate file during setup)
  val germanetFolder = path("SemanticResources", "GermaNetFolder")
  
  // intermediate files
  val targetsFile = "resources/targets.txt"
  val targetsPosFile = "resources/targets-pos.txt"
  val vocabFile = "resources/vocab.txt"
  val coocFile = "resources/coocs/germeval_coocs.tsv" // (generated from vocabFile and cooccurence corpus)
  
  // Defines complete processing
  lazy val preprocessing = Preprocessing(
      /*tokenizer = new Preprocessing.Tokenizer {
        lazy val model = new TokenizerME(new TokenizerModel(new File((path("Preprocessing", "opennlpPOSModel"))))
        def apply(sent: String) = model.tokenize(sent)
      },*/
      
      tokenizer = (s: String) => "[äöüÄÖÜß\\w]+".r.findAllIn(s).toVector, // still works best for German
      
      posTagger = new Preprocessing.PosTagger {
        lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "opennlpPOSModel"))))
        def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
      },
      
      lemmatizer = identity // no need
   )
   
  // load the germeval data (from cache, if available)
  lazy val germevalTraining = preprocessing.loadGermEval(germevalFolder, "train-dataset") 
  lazy val germevalTest = preprocessing.loadGermEval(germevalFolder, "test-dataset")
  
  // Candidate lists
  object candidates {
    lazy val germanet = new CandidateFile(path("Candidates", "germanet"), true)
    lazy val duden = new CandidateFile(path("Candidates", "duden"), true)
    lazy val woxikon = new CandidateFile(path("Candidates", "woxikon"), true)
    lazy val wortschatz = new CandidateFile(path("Candidates", "wortschatz"), true)
    lazy val masterlist = new CandidateFile(path("Candidates", "masterlist")  , true)
    lazy val goldCandidates = new CandidateFile("resources/candidates/germeval_gold.tsv" , true)
    
    // shortcut to select candidate lists
    lazy val trainingList = germanet
    lazy val systemList = germanet //new JoinedCandidates(germanet, wortschatz)
  }
  
  // N-gram counts
  object ngrams {
    lazy val web1t = Web1TLookup(path("NGrams", "web1t"), 5)
    lazy val germanWebCounts = Web1TLookup(path("NGrams", "germanWebCounts"), 5)
  }
  
  // Word embeddings
  object embeddings {
    val word2vecPruning = Integer.MAX_VALUE
    lazy val word2vec = Word2VecLookup(path("Embeddings", "word2vec"), word2vecPruning)
    lazy val eigenword = WordVectorFileLookup(path("Embeddings", "eigenwords"))
  }
  
  // DTs
  object dts {
    lazy val mateSim = DTLookup("de70M_mate_lemma", new WordSimilarityFile(path("DT", "mateSim"), identity, matchPrefix = true), 
        token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
        (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // how to define equivalence in this DT
        
    lazy val mateBims = DTLookup("de70M_mate_lemma_bims", new WordSimilarityFile(path("DT", "mateBims"), identity, matchPrefix = true), 
        token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
        (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma.toLowerCase + "#")) // no equivalence for bims
        
    lazy val trigramSim = DTLookup("de70M_trigram", new WordSimilarityFile(path("DT", "trigramSim"), identity), 
        token => token.lemma, // how to look up token in this DT
        (substitute, dtFeature) => dtFeature.startsWith(substitute.lemma)) // how to define equivalence in this DT
        
    lazy val trigramBims = DTLookup("de70M_trigram_bims", new WordSimilarityFile(path("DT", "trigramBims"), identity), 
        token => token.lemma, // how to look up token in this DT
        (substitute, dtFeature) => dtFeature.contains(substitute.lemma)) // how to define equivalence in this DT
  }
  
  // Co-occurence features
  lazy val cooc = DTLookup("cooc", new WordSimilarityFile(coocFile, identity), token => token.word)
  
  lazy val ngramCounts = ngrams.germanWebCounts
  
  // setup features
  lazy val features = new FeatureAnnotator(
    Cooc(cooc),
    WordSimilarity(dts.mateSim),
    WordSimilarity(dts.trigramSim),
    SalientDTFeatures(dts.trigramBims),
    SalientDTFeatures(dts.mateBims),
    BinaryWordSimilarity(dts.mateSim, 100),
    BinaryWordSimilarity(dts.trigramSim, 100),
    ThresholdedDTOverlap(dts.mateBims, Seq(5, 20, 50, 100, 200), false),
    ThresholdedDTOverlap(dts.mateSim, Seq(5, 20, 50, 100, 200), false),
    ThresholdedDTOverlap(dts.trigramBims, Seq(5, 20, 50, 100, 200), false),
    ThresholdedDTOverlap(dts.trigramSim, Seq(5, 20, 50, 100, 200), false),
    PosContextWindows(0 to 2, 0 to 2, 3),
    PairFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    SetFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    ConjunctionFreqRatio(ngramCounts, Seq("und", "oder", ","), 0, 0),
    LexSemRelation(candidates.masterlist),
    WordEmbeddingDistanceVectorsSet(embeddings.word2vec, 0 to 2, 0 to 2, 5),
    WordEmbeddingSimilarity(embeddings.word2vec)
  )
  
  // Others:
  // WordEmbeddingDistance(embeddings.word2vec)
}