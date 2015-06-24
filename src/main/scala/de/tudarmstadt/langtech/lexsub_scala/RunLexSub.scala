package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import com.googlecode.jweb1t.JWeb1TSearcher
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.distributional._
import de.tudarmstadt.langtech.lexsub_scala.germeval._
import de.tudarmstadt.langtech.lexsub_scala.features._
import de.tudarmstadt.langtech.lexsub_scala.features.{DTLookup, PosContextWindows, Web1TFreqRatios, LexSemRelation, WordEmbeddingDistanceVectors, WordEmbeddingSimilarity}
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel
import org.cleartk.classifier.jar.JarClassifierBuilder

/** Lexsub playground to train / run / evaluate / etc. */
object RunLexSub extends App {

  lazy val preprocessing = Preprocessing(
      //tokenizer = new Preprocessing.Tokenizer {
      //  val model = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
      //  def apply(sent: String) = model.tokenize(sent)
      //},
      tokenizer = (s: String) => "[äöüÄÖÜß\\w]+".r.findAllIn(s).toVector, // still works best for German
      posTagger = new Preprocessing.PosTagger {
        val tagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))
        def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
      },
      lemmatizer = identity
   )
   
  /* Preprocessed data can be trivially serialized */
  val data = utility.io.lazySerialized("germeval_cache.ser"){
    System.err.println("Cache does not exist, leading GermEval data..")
    val plainData = new GermEvalReader("../AIPHES_Data/GermEval2015", "train-dataset").items
    val processed = plainData.flatMap(preprocessing.tryApply)
    processed
  }
  
  /*
  val reader = new GermEvalResultOutcomeReader(data)
  reader.prettyPrint("instances.old.out")
  System.exit(0)
  */

  val TrainingDir = new File("training")
  val germanetFile = "../AIPHES_Data/LexSub/candidates/germeval_germanet.tsv"
  val masterlistFile = "../AIPHES_Data/LexSub/candidates/germeval_masterlist.tsv" //germeval_masterlist.tsv
  
  val embeddingFile = "../AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val dtfile = "../AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val web1tFolder = "../AIPHES_Data/web1t/de"
  val coocFile = "../AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt"
  
  val candidates = new CandidateFile(germanetFile, true)
  val masterlist = new CandidateFile(masterlistFile, true)

  val embedding = new WordVectorFile(embeddingFile)
  val web1t = new JWeb1TSearcher(new File(web1tFolder), 1, 5)
  
  val dt = DTLookup("de70M_mate_lemma", new WordSimilarityFile(dtfile, identity), 
      token => token.lemma.toLowerCase, 
      (substitute, other) => other.startsWith(substitute.toLowerCase))
  val cooc = DTLookup("cooc", new WordSimilarityFile(coocFile, identity), token => token.word)
  
  
  // setup features
  val features = new FeatureAnnotator(
      //WordSimilarity(dt),
      //WordSimilarity(cooc),
      ThresholdedDTOverlap(dt, Seq(20, 50, 100), false),
      PosContextWindows(0 to 2, 0 to 2, 3),
      Web1TFreqRatios(web1t, 0 to 2, 0 to 2, 5),
      Web1TConjunctionFreqRatio(web1t, Seq("und", "oder", ","), 0, 0),
      LexSemRelation(masterlist),
      WordEmbeddingDistanceVectors(embedding, 2, 2)
      //WordEmbeddingSimilarity(embedding),
      //WordEmbeddingDistance(embedding)
  )
  
  // train
  Training.train(data, candidates, features, TrainingDir)
  
  val lexsub = LexSubExpander(candidates, features, ClassifierScorer(TrainingDir))
  val outcomes = lexsub(data)
  
  val results = Training.collectOutcomes(data, outcomes)
  
  val outWriter = new GermEvalResultOutcomeWriter(results)
  outWriter.save("instances.out")
  println("Done.")
}