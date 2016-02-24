package de.tudarmstadt.langtech.lexsub_scala.utility

import java.io.PrintStream
import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import scala.sys.process.ProcessBuilder
import scala.sys.process.stringSeqToProcess
import ciir.umass.edu.learning.RankerFactory
import ciir.umass.edu.learning.Ranker
import scala.concurrent.Future
import scala.concurrent.ExecutionContext
import scala.concurrent.Await
import scala.concurrent.duration.Duration

/***
 *  This is a minimal command-line wrapper for RankLib
 *  As RankLib uses persistant state between calls, it is really best to "sandbox" it in its own process.
 *  
 *  As another benefit you get thread-safety when training multiple models in parallel
 */

/** Helper DSL for command line parameters to RankLib */
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
  
  def retrain(data: Iterable[RankEntry], config: RankLibConfig, trainingFilename: String): Future[Unit] = {
    // write LETOR FORMAT to tmpDataFile
    io.write(trainingFilename, RankLibWrapper.toLetorFormat(data))
    println("Wrote ranklib training data data in " + trainingFilename)
    RankLibWrapper.train(modelFile, trainingFilename, config)
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
  
  implicit val ec = ExecutionContext.global

  /** Runs RankLib as a seperate java process. Yields the output of RankLib as Future[String]
   *  @param printOutput if true, prints output to console rather than yielding it.
   */
	def runJava(params: Seq[String], printOutput: Boolean = false): Future[String] = {
			val mainClassName =  "ciir.umass.edu.eval.Evaluator"
					val currentJar = System.getProperty("java.class.path")
					val command: ProcessBuilder = Seq("java", "-cp", currentJar, mainClassName) ++ params
					
					val procFuture: Future[String] = Future {      
						if(printOutput)
							command.!.toString
						else
							command.!!
					}
          procFuture
	}

  /** Trains a model with the given config, yields model file path as future */
  def train(modelFile: String, trainingFile: String, config: RankLibConfig): Future[Unit] = {
		  /* example params:
		   * -train MQ2008/Fold1/train.txt -test MQ2008/Fold1/test.txt -validate MQ2008/Fold1/vali.txt 
		   * -ranker 6 -metric2t NDCG@10 -metric2T ERR@10 -save mymodel.txt o*/
		  val procFuture = RankLibWrapper.runJava(Seq(
				  "-train", trainingFile, 
				  //"-validate", trainingFile, // specify training as validation, so we get early stopping!
				  "-save", modelFile) ++ config.asArguments, printOutput = true)

				  procFuture.onSuccess { case _ =>
				    println(s"Completed training RankLib on $trainingFile, wrote to $modelFile")
		      }
		  procFuture.map(_ => ())
  }
  
  def rank(modelFile: String, dataFile: String, outFile: String, metric: String = "ERR@10") {
    val f = RankLibWrapper.runJava(Seq(
        "-load", modelFile, 
        "-rank", dataFile, 
        //"-metric2T", metric
        "-score", outFile), printOutput = false)
     Await.result(f, Duration.Inf)
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
  io.withTmpFile { tmp => 
    ranker.retrain(data, LambdaMart(NDCG(10), 10), tmp)
  }
  println("ranking..")
  println(ranker.rank(data))
}