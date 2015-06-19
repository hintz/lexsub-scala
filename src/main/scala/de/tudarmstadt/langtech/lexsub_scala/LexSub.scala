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
import de.tudarmstadt.langtech.lexsub_scala.training.TrainingDataCreation
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


case class ClassifierScorer(val trainingDiretory: File) {
  
  import org.cleartk.classifier.mallet.MalletStringOutcomeClassifierBuilder
  import org.cleartk.classifier.Classifier
  import org.cleartk.classifier.Feature
    
  val GoodLabel = "GOOD"
  val classifier = JarClassifierBuilder
    .fromTrainingDirectory(trainingDiretory)
    .loadClassifierFromTrainingDirectory(trainingDiretory)
    .asInstanceOf[Classifier[String]]
  
  def apply(features: Seq[Feature]): Double = {
    val outcomes = classifier.score(features, 2)
    val goodScore = outcomes.collectFirst { case o if o.getOutcome == GoodLabel => o.getScore }
    goodScore.getOrElse(throw new IllegalStateException)
  }
}

case class LexSubExpander(
    candidateList: CandidateList, 
    featureAnnotator: FeatureAnnotator, 
    scorer: ClassifierScorer, 
    maxItems: Int = 10) {
  
  /** Expands and ranks */
  def apply(instance: LexSubInstance): Seq[(String, Double)] = {
    val candidates = candidateList(instance.head.lemma)
    apply(instance, candidates)
  }
  
  /** Ranks predefined substitution candidates */
  def apply(instance: LexSubInstance, substitutions: Seq[String]): Seq[(String, Double)] = {
    val substItems = substitutions.map(new SubstitutionItem(instance, _))
    val scored = substitutions.zip(substItems).map { case (subst, substItem) =>
      val features = featureAnnotator.annotate(substItem)
      val score = scorer(features)
      (subst, score)
    }
    scored.sortBy(- _._2).take(maxItems)
  }
}


object LexSub extends App {
  val TrainingDir = new File("training")

  //val web1t = new JWeb1TSearcher(new File("/Volumes/AIPHES_HDD/AIPHES_Data/web1t/de"), 1, 5)
  //val freq = web1t.getFrequency("Dies ist ein")
  //println(freq)
  
  //val gn: GermaNet = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  //val gehen = gn.getSynsets("gehen")
  //println(gehen)
  
  //val opennlpTokenizer = new TokenizerME(new TokenizerModel(new File("resources/models/opennlp/de-token.bin")))
 
  //val postagger = new POSTaggerME(new POSModel(new File("resources/models/opennlp/de-pos-perceptron.bin")))
 
  //val tokens = opennlpTokenizer.tokenize("Das ist ein Test")
  
  //val tags = postagger.tag(tokens)
  
  
  val candidates = new CandidateFile("../lexsub-gpl/AIPHES_Data/LexSub/candidates/germeval_masterlist.tsv", semanticRelationColumn = true)
  val data = new GermEvalReader("../lexsub-gpl/AIPHES_Data/GermEval2015", "train-dataset").items.take(30)
  
  val processed = data.map(Preprocess.apply)
  
  val trainingData = TrainingDataCreation.apply(processed, candidates, false)
  
  val embeddingFile = "../lexsub-gpl/AIPHES_Data/WordEmbeddings/eigenwords.300k.200.de.sorted"
  val dtfile = "../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub"

  val eigentwortCossim = new WordEmbeddingSimilarity("eigenwort_cossim", new WordVectorFile(embeddingFile))
  val dt = new DTLookup("de70M_mate_lemma", new DTFile(dtfile, identity), token => token.lemma.toLowerCase)
  
  // setup features
  val annotator = new FeatureAnnotator(
      new ThresholdedDTOverlap(dt, Seq(10, 20, 100), false),
      eigentwortCossim
  )
  
  // map to instances
  val instances = annotator(trainingData)
  val dataWriter = new MalletStringOutcomeDataWriter(TrainingDir)
  
  
  /*
  val writer = new InstanceDataWriter[String](TrainingDir)
  instances foreach dataWriter.write
  dataWriter.finish
  
  //JarClassifierBuilder.trainAndPackage(TrainingDir, "MaxEnt")
  
  dataWriter.getClassifierBuilder.trainClassifier(TrainingDir, "MaxEnt")
  dataWriter.getClassifierBuilder.packageClassifier(TrainingDir)
  */
  //val classifier = dataWriter.getClassifierBuilder.loadClassifierFromTrainingDirectory(TrainingDir)
  
  //val results = classifier.score(instances.head.getFeatures, 10)
  //results foreach println
  
  
  val lexsub = LexSubExpander(candidates, annotator, ClassifierScorer(TrainingDir))
  
  val firstItem = lexsub.apply(processed.head)
  firstItem foreach println
  
  //val model = new TokenizerModel(is)
  //val gnr = new GermaNetResource("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
}