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
  
  // folder to store training data and trained model
  val trainingDir = "training"
  
  // output file
  val instancesOutputFile = "instances.out"
  
  // GermaNet location (only needed to create candidate file during setup)
  val germanetFolder = "../AIPHES_Data/GermaNet/GN_V90/GN_V90_XML"
  
  // candidate lists
  val germanetFile = "resources/candidates/germeval_germanet90.tsv"
  val dudenFile = "resources/candidates/germeval_duden.tsv"  
  val masterlistFile = "resources/candidates/germeval_masterlist.tsv"
  
  val embeddingFile = "../AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val word2vecFile = "../AIPHES_Data/WordEmbeddings/word2vec/denews-vectors.bin"
  
  // some DTs:
  val dt1Bims = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_BIM_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_sorted"
  val dt1Similar = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val dt2Bims = "../AIPHES_Data/DT/de70M_trigram/de70M_trigram_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_filtered_g1_sorted"
  val dt2Similar = "../AIPHES_Data/DT/de70M_trigram/de70M_trigram_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_sorted"
  
  val coocFile = "../AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt"
  
  val web1tFolder = "../AIPHES_Data/web1t/de"
  val germanWebCountsFolder = "../AIPHES_Data/NGrams/GermanWebCounts"
  
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
   
  // load the germeval data (from cache, if available)
  lazy val germevalTraining = preprocessing.loadGermEval(germevalFolder, "train-dataset") 
  lazy val germevalTest = preprocessing.loadGermEval(germevalFolder, "test-dataset") 
  
  // Candidate lists
  object candidates {
    lazy val germanet = new CandidateFile(germanetFile, true)
    lazy val duden = new CandidateFile(dudenFile, true)
    lazy val masterlist = new CandidateFile(masterlistFile, true)
  }

  
  // N-gram counts
  object ngrams {
    lazy val web1t = Web1TLookup(web1tFolder, 5)
    lazy val germanWebCounts = Web1TLookup(germanWebCountsFolder, 5)
  }
  
  // Word embeddings
  object embeddings {
    val word2vecPruning = Integer.MAX_VALUE
    lazy val word2vec = Word2VecLookup(word2vecFile, word2vecPruning)
    lazy val eigenword = WordVectorFileLookup(embeddingFile)
  }
  
  
  // DTs
  object dts {
    lazy val dt1 = DTLookup("de70M_mate_lemma", new WordSimilarityFile(dt1Similar, identity), 
        token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
        (substitute, other) => other.startsWith(substitute.toLowerCase)) // how to define equivalence in this DT
        
    lazy val dt1_bims = DTLookup("de70M_mate_lemma_bims", new WordSimilarityFile(dt1Bims, identity), 
        token => token.lemma.toLowerCase + "#" + token.pos.take(2).toUpperCase, // how to look up token in this DT
        (substitute, other) => throw new IllegalStateException) // no equivalence for bims
        
    lazy val dt2 = DTLookup("de70M_trigram", new WordSimilarityFile(dt2Similar, identity), 
        token => token.lemma, // how to look up token in this DT
        (substitute, other) => other.startsWith(substitute)) // how to define equivalence in this DT
        
    lazy val dt2_bims = DTLookup("de70M_trigram_bims", new WordSimilarityFile(dt2Bims, identity), 
        token => token.lemma, // how to look up token in this DT
        (substitute, other) => throw new IllegalStateException) // how to define equivalence in this DT
  }
  
  // Co-occurence features
  lazy val cooc = DTLookup("cooc", new WordSimilarityFile(coocFile, identity), token => token.word)
  
  // setup features
  val features = new FeatureAnnotator(
      //CheatFeature,
      //WordSimilarity(dt),
      Cooc(cooc),
      ThresholdedDTOverlap(dts.dt1, Seq(5, 20, 50, 100, 200), false),
      //ThresholdedDTOverlap(dts.dt2, Seq(5, 20, 50, 100, 200), false),
      //ThresholdedDTOverlap(dts.dt1_bims, Seq(5, 20, 50, 100, 200), false),
      //ThresholdedDTOverlap(dts.dt2_bims, Seq(5, 20, 50, 100, 200), false),
      PosContextWindows(0 to 2, 0 to 2, 3),
      PairFreqRatios(ngrams.web1t, 0 to 2, 0 to 2, 5),
      SetFreqRatios(ngrams.web1t, 0 to 2, 0 to 2, 5),
      ConjunctionFreqRatio(ngrams.web1t, Seq("und", "oder", ","), 0, 0),
      LexSemRelation(candidates.masterlist)
      //WordEmbeddingDistanceVectorsSet(embeddings.word2vec, 0 to 2, 0 to 2, 5),
      //WordEmbeddingSimilarity(embeddings.word2vec),
      //WordEmbeddingDistance(embeddings.word2vec)
  )
}