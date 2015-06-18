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

object LexSub extends App {

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
  trainingData.map(_.isGood) foreach println
  
  
  //val model = new TokenizerModel(is)
  //val gnr = new GermaNetResource("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
}