package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.reader._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features.{ Cooc, EditDistance, DTLookup, SalientDTFeatures, BinaryWordSimilarity, PosContextWindows, PairFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity, Word2VecLookup }
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.scala_utilities.formatting.de.tudarmstadt.langtech.scala_utilities.formatting.YamlSettings
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

/** Nearly all of lexsub-scala can be configured in this file */
object Settings extends YamlSettings("semeval2007-paths.yaml") {

  val semevalFolder = path("semevalFolder")
  val resourcesFolder = path("resourcesFolder")
  val trainingDir = path("trainingFolder")
  val instancesOutputFile = path("outputFile")

  // intermediate files
  val targetsFile = resourcesFolder + "/targets.txt"
  val targetsPosFile = resourcesFolder + "/targets-pos.txt"
  val vocabFile = resourcesFolder + "/vocab.txt"

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

    
  // plain data
  val trialReader = new SemEvalReader(semevalFolder, "trial/lexsub_trial.xml", "trial/gold.trial")
  val testReader = new SemEvalReader(semevalFolder, "test/lexsub_test.xml", "test/gold.gold")
  
  // parsed daat
  val semevalTrial = io.lazySerialized("cache/semeval_trial.ser") { preprocessing.parseSemEval(trialReader.items) }
  val semevalTest = io.lazySerialized("cache/semeval_test.ser") { preprocessing.parseSemEval(testReader.items) }
  
  // Candidate lists
  object candidates {
    lazy val wordnet = new CandidateFile(path("Candidates", "wordnet"), true)
    lazy val masterlist = new CandidateFile(path("Candidates", "masterlist"), true)
    lazy val gold = new CandidateFile(path("Candidates", "gold"), true)

    // shortcut to select candidate lists
    lazy val trainingList = wordnet
    lazy val systemList = wordnet
  }

  // N-gram counts
  object ngrams {
    lazy val web1t = Web1TLookup(path("NGrams", "web1t"), 5)
  }

  // Word embeddings
  object embeddings {
    val word2vecPruning = Integer.MAX_VALUE
    lazy val levyWords = WordVectorFileLookup(path("Embeddings", "levyWords"))
    lazy val levyContexts = WordVectorFileLookup(path("Embeddings", "levyContexts"))
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
 //lazy val cooc = DTLookup("cooc", 
 //    new WordSimilarityFile(path("Coocs", "semeval"), _.takeWhile(_ != '/')), 
 //    lookupFunction = token => token.word)

  lazy val ngramCounts = ngrams.web1t

  // setup features
  lazy val features = new FeatureAnnotator(
    //SentenceIDFeature,
    //SubstitutionFeature,
    //Cooc(cooc),
    SalientDTFeatures(dts.firstOrder),
    WordSimilarity(dts.secondOrder),
    BinaryWordSimilarity(dts.secondOrder, 100),
    AllThresholdedDTFeatures(
      Seq(dts.secondOrder),
      Seq(5, 20, 50, 100, 200)),
    PosContextWindows(0 to 1, 0 to 1, 3),
    PairFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    SetFreqRatios(ngramCounts, 0 to 2, 0 to 2, 5),
    ConjunctionFreqRatio(ngramCounts, Seq("and", "or", ","), 0, 0),
    NumLexSemRelations(candidates.systemList),
    LexSemRelation(candidates.systemList) 
    )
  // Others:
  // WordEmbeddingDistanceVectorsSet(embeddings.word2vec, 0 to 2, 0 to 2, 5),
  // WordEmbeddingSimilarity(embeddings.word2vec)
  // WordEmbeddingDistance(embeddings.word2vec)
  // EditDistance
}
