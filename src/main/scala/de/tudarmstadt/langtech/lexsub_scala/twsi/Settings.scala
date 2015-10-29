package de.tudarmstadt.langtech.lexsub_scala.twsi

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.reader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features.{ Cooc, EditDistance, DTLookup, SalientDTFeatures, BinaryWordSimilarity, PosContextWindows, PairFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity, Word2VecLookup }
import de.tudarmstadt.langtech.lexsub_scala.types.Preprocessing
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.scala_utilities.formatting.de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("twsi-paths.yaml") {

  val twsiFolder = path("twsiFolder")

  // folder to store training data and trained model
  val trainingDir = path("trainingFolder")

  // output file
  val instancesOutputFile = path("outputFile")

  // intermediate files
  val targetsFile = "resources/targets.txt"
  val targetsPosFile = "resources/targets-pos.txt"
  val vocabFile = "resources/vocab.txt"
  val coocFile = "resources/coocs/germeval_coocs.tsv" // (generated from vocabFile and cooccurence corpus)

  // Defines complete processing
  implicit lazy val preprocessing = Preprocessing(
    tokenizer = new Preprocessing.Tokenizer {
      lazy val model = new TokenizerME(new TokenizerModel(new File((path("Preprocessing", "opennlpTokenModel")))))
      def apply(sent: String) = model.tokenize(sent)
    },

    posTagger = new Preprocessing.PosTagger {
      lazy val tagger = new POSTaggerME(new POSModel(new File(path("Preprocessing", "opennlpPOSModel"))))
      def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
    },

    lemmatizer = identity // no need
    )

  val semevalData = io.lazySerialized("cache_twsi_data.ser") {
    new SemEvalReader(twsiFolder, "twsi2.xml", "twsi2.gold").items
  }

  // parse the data
  lazy val lexsubData = io.lazySerialized("cache_twsi_parsed.ser") {
    preprocessing.parseSemEval(semevalData)
  }

  // Candidate lists
  object candidates {
    lazy val masterlist = new CandidateFile(path("Candidates", "masterlist"), true)

    // shortcut to select candidate lists
    lazy val trainingList = masterlist
    lazy val systemList = masterlist
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

  lazy val ngramCounts = ngrams.web1t

  // setup features
  lazy val features = new FeatureAnnotator(
    Cooc(cooc),
    WordSimilarity(dts.mateSim),
    WordSimilarity(dts.trigramSim),
    SalientDTFeatures(dts.trigramBims),
    SalientDTFeatures(dts.mateBims),
    BinaryWordSimilarity(dts.mateSim, 100),
    BinaryWordSimilarity(dts.trigramSim, 100),
    AllThresholdedDTFeatures(
      Seq(dts.mateBims, dts.mateSim, dts.trigramBims, dts.trigramSim),
      Seq(5, 20, 50, 100, 200)),
    PosContextWindows(0 to 1, 0 to 1, 3),
    PairFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    SetFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    ConjunctionFreqRatio(ngramCounts, Seq("und", "oder", ","), 0, 0),
    NumLexSemRelations(candidates.masterlist),
    LexSemRelation(candidates.masterlist) //WordEmbeddingDistanceVectorsSet(embeddings.word2vec, 0 to 2, 0 to 2, 5),
    //WordEmbeddingSimilarity(embeddings.word2vec)
    )
  // Others:
  // WordEmbeddingDistance(embeddings.word2vec)
  // EditDistance
}
