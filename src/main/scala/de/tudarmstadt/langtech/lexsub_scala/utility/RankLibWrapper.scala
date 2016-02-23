package de.tudarmstadt.langtech.lexsub_scala.utility

import java.io.PrintStream
import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import scala.sys.process.ProcessBuilder
import scala.sys.process.stringSeqToProcess
import ciir.umass.edu.learning.RankerFactory
import ciir.umass.edu.learning.Ranker


trait RankLibConfig {
  def asArguments: Seq[String]
}

trait Metric
case class NDCG(n: Int) extends Metric { override def toString = "NDCG@" + n }
case class ERR(n: Int) extends Metric { override def toString = "ERR@" + n }

case class LambdaMart(metric: Metric, numIterations: Int) extends RankLibConfig {
  def asArguments = Seq("-ranker", "6", "-tree", numIterations.toString, "-metric2t", metric.toString)
}

case class RankEntry(val queryId: Int, val docID: String, val relevanceScore: Int, val features: List[(Int, Double)])
case class RankResult(val docID: Int, val score: Double)

class RankLibWrapper(val modelFile: String){
  
  def retrain(data: Iterable[RankEntry], config: RankLibConfig, trainingFilename: String){
    // write LETOR FORMAT to tmpDataFile
    io.write(trainingFilename, RankLibWrapper.toLetorFormat(data))
    println("Wrote ranklib training data data in " + trainingFilename)
    RankLibWrapper.train(modelFile, trainingFilename, config)
    println("Done training ranker in " + modelFile)
  }
  
  def rank(data: Iterable[RankEntry]): Map[Int, List[Double]] = {
    val tmpOutFile = File.createTempFile(modelFile, ".rank.tmp")
    val tmpOutPath = tmpOutFile.getAbsolutePath
    val tmpDataFile = File.createTempFile("data", ".tmp")
    
    // write LETOR FORMAT to tmpDataFile
    io.write(tmpDataFile.getAbsolutePath, RankLibWrapper.toLetorFormat(data))
    System.err.println("Wrote temporary LETOR ranking data to " + tmpDataFile)
    
    // call ranking with temporary files
    RankLibWrapper.rank(modelFile, tmpDataFile.getAbsolutePath, tmpOutPath)
    
    val output = io.lines(tmpOutPath).map(_.split("\t")).map { 
      case Array(queryId, docId, score) => (queryId.toInt, docId.toInt, score.toDouble)
    }.toList
    
    val scoresPerQuery = output.groupBy(_._1).mapValues { ordered => ordered.map(x => x._3) }
    tmpOutFile.delete
    tmpDataFile.delete
    scoresPerQuery
  }
}

object RankLibWrapper {
  
  def runJava(params: Seq[String], printOutput: Boolean = false){
      val mainClassName =  "ciir.umass.edu.eval.Evaluator"
      val currentJar = System.getProperty("java.class.path")
      val command: ProcessBuilder = Seq("java", "-cp", currentJar, mainClassName) ++ params
      
      if(printOutput)
        command.!
      else
        command.!!
  }
  
  /** Exectues a block with a temporary file, which is then deleted */
  def withTmpFile(block: String => Unit) = {
    val tmpFile = File.createTempFile("temporary", "tmp")
    val tmpPath = tmpFile.getAbsolutePath
    block(tmpPath)
    tmpFile.delete
  }
  
  /** Exectues a block without printing stdout to console */
  def noOutput(block: => Unit){
      val original = System.out
      val nullStream = new PrintStream(new java.io.OutputStream { def write(b: Int) {} })
      System.setOut(nullStream)
      block
      System.setOut(original)
  }
  
  def train(modelFile: String, trainingFile: String, config: RankLibConfig) = {
      /* example params:
       * -train MQ2008/Fold1/train.txt -test MQ2008/Fold1/test.txt -validate MQ2008/Fold1/vali.txt 
       * -ranker 6 -metric2t NDCG@10 -metric2T ERR@10 -save mymodel.txt
       * 
       * 
       */
      RankLibWrapper.runJava(Seq(
          "-train", trainingFile, 
          //"-validate", trainingFile, // specify training as validation, so we get early stopping!
          "-save", modelFile) ++ config.asArguments, printOutput = true)
  }
  
  def rank(modelFile: String, dataFile: String, outFile: String, metric: String = "ERR@10") = noOutput {
    RankLibWrapper.runJava(Seq(
        "-load", modelFile, 
        "-rank", dataFile, 
        //"-metric2T", metric
        "-score", outFile))
  }
  
  

  def toLetorFormat(data: Iterable[RankEntry]): String = {
    /* LETOR format: 
     * Each row is a query-document pair. 
     * The first column is relevance label of this pair, the second column is query id, the following columns
     * are features, and the end of the row is comment about the pair, including id of the document. */  
    val lines = data.map(toLetorLine)
    lines.mkString("\n")
  }
  
  def toLetorLine(rankEntry: RankEntry): String = rankEntry match {
    case RankEntry(queryId, docID, relevanceScore, features) =>
      val featurePairs = features.toList.map { case (featureName, value) => featureName + ":" + value }
      s"$relevanceScore qid:$queryId ${featurePairs.mkString(" ")} # docId:$docID"
   }
}




object TestRankLibWrapper extends App {
  
  //val rFact = new RankerFactory
  //val r: Ranker = rFact.loadRanker("foo.txt")

  RankLibWrapper.train("foo.txt", "training.txt", LambdaMart(NDCG(10), 10))  
  
  //RankLibWrapper.train("mymodel.txt", "MQ2008/Fold1/train.txt")
  //RankLibWrapper.rank("mymodel.txt", "MQ2008/Fold2/train.txt", "out.tmp")
  
  
  val data = List(
		  RankEntry(1, "42", 0, List((0, 0.5), (1, 1.0))),
		  RankEntry(1, "43", 1, List((0, 0.3), (1, 0.1))),
		  RankEntry(1, "44", 3, List((0, 1.0), (1, 1.0))),
		  RankEntry(1, "45", 4, List((0, 1.5), (1, 0.2)))
  )
      
  val ranker = new RankLibWrapper("test.ranker.txt")
  RankLibWrapper.withTmpFile { tmp => 
    ranker.retrain(data, LambdaMart(NDCG(10), 10), tmp)
  }
  println("ranking..")
  println(ranker.rank(data))
}