package de.tudarmstadt.langtech.lexsub_scala

import java.io.File
import com.googlecode.jweb1t.JWeb1TSearcher
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordSimilarityFile
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordVectorFile
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.features.PosContextWindows
import de.tudarmstadt.langtech.lexsub_scala.features.ThresholdedDTOverlap
import de.tudarmstadt.langtech.lexsub_scala.features.Web1TFreqRatios
import de.tudarmstadt.langtech.lexsub_scala.features.WordSimilarity
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import opennlp.tools.postag.POSModel
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.tokenize.TokenizerModel

/** Lexsub playground to train / run / evaluate / etc. */
object LexSub extends App {

  lazy val preprocessing = Preprocessing(
      tokenizer = new Preprocessing.Tokenizer {
        val model = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
        def apply(sent: String) = model.tokenize(sent)
      },
      posTagger = new Preprocessing.PosTagger {
        val tagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))
        def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
      },
      lemmatizer = identity
   )
   
  /* Preprocessed data can be trivially serialized */
  val data = utility.io.lazySerialized("germeval_cache.ser"){
    val plainData = new GermEvalReader("../lexsub-gpl/AIPHES_Data/GermEval2015", "train-dataset").items
    val processed = plainData.flatMap(preprocessing.tryApply)
    processed
  }

  val TrainingDir = new File("training")
  val masterlistFile = "../lexsub-gpl/AIPHES_Data/LexSub/candidates/germeval_masterlist.tsv"
  
  val embeddingFile = "../lexsub-gpl/AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val dtfile = "../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val web1tFolder = "../lexsub-gpl/AIPHES_Data/web1t/de"
  val coocFile = "../lexsub-gpl/AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt"
  
  val candidates = new CandidateFile(masterlistFile, semanticRelationColumn = true)

  val embedding = new WordVectorFile(embeddingFile)
  val web1t = new JWeb1TSearcher(new File(web1tFolder), 1, 5)
  
  val dt = DTLookup("de70M_mate_lemma", new WordSimilarityFile(dtfile, identity), 
      token => token.lemma.toLowerCase, 
      (substitute, other) => other.startsWith(substitute.toLowerCase))
  val cooc = DTLookup("cooc", new WordSimilarityFile(coocFile, identity), token => token.word)
  
  
  // setup features
  val features = new FeatureAnnotator(
      WordSimilarity(dt),
      WordSimilarity(cooc),
      ThresholdedDTOverlap(dt, Seq(10, 20, 100), false),
      PosContextWindows(0 to 2, 0 to 2),
      Web1TFreqRatios(web1t, 0 to 2, 0 to 2)
      //LexSemRelation(candidates),
      //WordEmbeddingDistanceVectors(embedding, 2, 2),
      //WordEmbeddingSimilarity(embedding),
      //WordEmbeddingDistance(embedding)
  )
  
  // train
  Training.train(data, candidates, features, TrainingDir)
  
  val lexsub = LexSubExpander(candidates, features, ClassifierScorer(TrainingDir))
  val outcomes = data.map(lexsub.apply)
  
  val results = Training.collectOutcomes(data, outcomes)
  
  val outWriter = new GermEvalResultOutcomeWriter(results)
  outWriter.save("instances.out")
}