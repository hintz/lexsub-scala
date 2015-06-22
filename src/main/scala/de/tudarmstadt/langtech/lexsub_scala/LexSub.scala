package de.tudarmstadt.langtech.lexsub_scala

import de.tuebingen.uni.sfs.germanet.api.GermaNet
import com.googlecode.jweb1t.JWeb1TSearcher
import java.io.File
import opennlp.tools.tokenize.TokenizerModel
import opennlp.tools.tokenize.TokenizerFactory
import opennlp.tools.tokenize.TokenizerME
import opennlp.tools.postag.POSTaggerME
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalReader
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.features.ThresholdedDTOverlap
import de.tudarmstadt.langtech.lexsub_scala.distributional.DTFile
import de.tudarmstadt.langtech.lexsub_scala.features.DTLookup
import de.tudarmstadt.langtech.index_file.PrefixIndexedFile
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.distributional.WordVectorFile
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingSimilarity
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import org.cleartk.classifier.encoder.features.NameNumberFeaturesEncoder
import org.cleartk.classifier.encoder.features.NumberEncoder
import org.cleartk.classifier.encoder.features.BooleanEncoder
import org.cleartk.classifier.encoder.features.StringEncoder
import org.cleartk.classifier.encoder.outcome.StringToStringOutcomeEncoder
import scala.collection.JavaConversions._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.training.Training
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.features.PosContextWindow
import de.tudarmstadt.langtech.lexsub_scala.features.LexSemRelation
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistanceVectors
import de.tudarmstadt.langtech.lexsub_scala.features.WordEmbeddingDistance
import de.tudarmstadt.langtech.lexsub_scala.features.PosContextWindows
import de.tudarmstadt.langtech.lexsub_scala.features.Web1TFreqRatio

object LexSub extends App {
  val TrainingDir = new File("training")
  
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

  
  //val gn: GermaNet = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  //val gehen = gn.getSynsets("gehen")
  //println(gehen)
  
  //val opennlpTokenizer = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
 
  //val postagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))
 
  //val tokens = opennlpTokenizer.tokenize("Das ist ein Test")
  
  //val tags = postagger.tag(tokens)
  
   //val model = new TokenizerModel(is)
   //val gnr = new GermaNetResource("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  
  val candidates = new CandidateFile("../lexsub-gpl/AIPHES_Data/LexSub/candidates/germeval_masterlist.tsv", semanticRelationColumn = true)
  
  val data = new GermEvalReader("../lexsub-gpl/AIPHES_Data/GermEval2015", "train-dataset").items.take(10)
  
  val processed = data.flatMap(preprocessing.tryApply)
    
  /* Preprocessed data can be trivially serialized */
  //io.deserialize[LexSubInstance]("data.ser")
  //io.serialize("data.ser", processed)
  
  val embeddingFile = "../lexsub-gpl/AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val dtfile = "../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"
  val web1tFolder = "../lexsub-gpl/AIPHES_Data/web1t/de"
  
  
  val embedding = new WordVectorFile(embeddingFile)
  
  val web1t = new JWeb1TSearcher(new File(web1tFolder), 1, 5)
  
  val dt = new DTLookup("de70M_mate_lemma", new DTFile(dtfile, identity), token => token.lemma.toLowerCase)
  
  // setup features
  val features = new FeatureAnnotator(
      //ThresholdedDTOverlap(dt, Seq(10, 20, 100), false),
      //PosContextWindows(0 to 2, 0 to 2),
      //LexSemRelation(candidates),
      Web1TFreqRatio(web1t, 1, 1)
      //WordEmbeddingDistanceVectors(embedding, 2, 2),
      //WordEmbeddingSimilarity(embedding),
      //WordEmbeddingDistance(embedding)
  )
  
  // train
  //Training.train(processed, candidates, features, TrainingDir)
  
  val lexsub = LexSubExpander(candidates, features, ClassifierScorer(TrainingDir))
  val outcomes = processed.map(lexsub.apply)
  
  val results = Training.collectOutcomes(processed, outcomes)
  
  val outWriter = new GermEvalResultOutcomeWriter(results)
  outWriter.save("instances.out")
}