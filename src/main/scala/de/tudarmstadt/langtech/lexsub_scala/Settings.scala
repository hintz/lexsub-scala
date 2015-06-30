package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.germeval._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.filereader._
import de.tudarmstadt.langtech.lexsub_scala.features.{DTLookup, PosContextWindows, PairFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity}
import de.tudarmstadt.langtech.lexsub_scala.types.Preprocessing
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel

/** Nearly all of lexsub-scala can be configured in this file */
object Settings {
  
  val germevalFolder = "../AIPHES_Data/GermEval2015"
  
  val trainingDir = "training"
  val germanetFile = "../AIPHES_Data/LexSub/candidates/germeval_germanet.tsv"
  val masterlistFile = "../AIPHES_Data/LexSub/candidates/germeval_masterlist.tsv"
  
  val embeddingFile = "../AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val word2vecFile = "../AIPHES_Data/WordEmbeddings/word2vec/denews-vectors.bin"
  
  val dtfile = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val dt2file = "../AIPHES_Data/DT/..."
  val coocFile = "../AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt"
  
  val web1tFolder = "../AIPHES_Data/web1t/de"
  val germanWebCountsFolder = "../AIPHES_Data/NGrams/GermanWebCounts"
  
  val instancesOutputFile = "instances.out"
  
  // Defines complete processing
  lazy val preprocessing = Preprocessing(
      /*tokenizer = new Preprocessing.Tokenizer {
        val model = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
        def apply(sent: String) = model.tokenize(sent)
      },*/
      
      tokenizer = (s: String) => "[äöüÄÖÜß\\w]+".r.findAllIn(s).toVector, // still works best for German
      
      posTagger = new Preprocessing.PosTagger {
        val tagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))
        def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
      },
      
      lemmatizer = identity // no need
   )
   
  lazy val germevalTraining = {
    /* Preprocessed data can be trivially serialized */
    println("Loading GermEval data..")
     io.lazySerialized("germeval_cache.ser"){
      System.err.println("Cache does not exist, leading GermEval data..")
      val plainData = new GermEvalReader(germevalFolder, "train-dataset").items
      val processed = plainData.flatMap(Settings.preprocessing.tryApply)
      processed
    }
  }
  
  // Candidate lists
  lazy val candidates = new CandidateFile(germanetFile, true)
  lazy val masterlist = new CandidateFile(masterlistFile, true)
  
  // N-gram counts
  lazy val web1t = Web1TLookup(web1tFolder, 5)
  lazy val germanWebCounts = Web1TLookup(germanWebCountsFolder, 5)
  
  // Word embeddings
  lazy val word2vecEmbedding = Word2VecLookup(word2vecFile)
  lazy val eigenwordEmbedding = WordVectorFileLookup(embeddingFile)
  
  // DTs
  lazy val dt1 = DTLookup("de70M_mate_lemma", new WordSimilarityFile(dtfile, identity), 
      token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
      (substitute, other) => other.startsWith(substitute.toLowerCase)) // how to define equivalence in this DT

  // Co-occurence features
  lazy val cooc = DTLookup("cooc", new WordSimilarityFile(coocFile, identity), token => token.word)
  
  // setup features
  val features = new FeatureAnnotator(
      //CheatFeature,
      //WordSimilarity(dt),
      Cooc(cooc),
      ThresholdedDTOverlap(dt1, Seq(5, 20, 50, 100, 200), false),
      PosContextWindows(0 to 2, 0 to 2, 3),
      PairFreqRatios(web1t, 0 to 2, 0 to 2, 5),
      SetFreqRatios(web1t, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(web1t, Seq("und", "oder", ","), 0, 0),
      LexSemRelation(masterlist)
      //WordEmbeddingDistanceVectorsSet(word2vecEmbedding, 0 to 2, 0 to 2, 5),
      //WordEmbeddingSimilarity(word2vecEmbedding),
      //WordEmbeddingDistance(word2vecEmbedding)
  )
}