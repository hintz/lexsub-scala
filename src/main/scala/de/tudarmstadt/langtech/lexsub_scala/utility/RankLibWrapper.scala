package de.tudarmstadt.langtech.lexsub_scala.utility

import ciir.umass.edu.eval.Evaluator
import java.io.PrintStream
import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io

case class RankEntry(val queryId: Int, val docID: String, val relevanceScore: Int, val features: List[(Int, Double)])
case class RankResult(val docID: Int, val score: Double)

class RankLibWrapper(val modelFile: String){
  
  def retrain(data: Iterable[RankEntry], trainingFilename: String){
    // write LETOR FORMAT to tmpDataFile
    io.write(trainingFilename, RankLibWrapper.toLetorFormat(data))
    println("Wrote ranklib training data data in " + trainingFilename)
    RankLibWrapper.train(modelFile, trainingFilename)
    println("Done training ranker in " + modelFile)
  }
  
  def rank(data: Iterable[RankEntry]): Map[Int, List[Double]] = {
    val tmpOutFile = File.createTempFile(modelFile, ".rank.tmp")
    val tmpOutPath = tmpOutFile.getAbsolutePath
    val tmpDataFile = File.createTempFile("data", ".tmp")
    
    // write LETOR FORMAT to tmpDataFile
    io.write(tmpDataFile.getAbsolutePath, RankLibWrapper.toLetorFormat(data))
    println("letor data written to " + tmpDataFile)
    
    // call ranking with temporary files
    RankLibWrapper.rank(modelFile, tmpDataFile.getAbsolutePath, tmpOutPath)
    
    val output = io.lines(tmpOutPath).map(_.split("\t")).map { 
      case Array(queryId, docId, score) => (queryId.toInt, docId.toInt, score.toDouble)
    }.toList
    
    //val rankingsPerQuery = output.groupBy(_._1).mapValues { unsorted =>
    //  unsorted.map(x => RankResult(x._2, x._3)).sortBy(- _.score)
    //}
    
    val scoresPerQuery = output.groupBy(_._1).mapValues { ordered => ordered.map(x => x._3) }
    tmpOutFile.delete
    tmpDataFile.delete
    scoresPerQuery
  }
}

object RankLibWrapper {
  
  val DisableStdout = true
  
  /** Exectues a block with a temporary file, which is then deleted */
  def withTmpFile(block: String => Unit) = {
    val tmpFile = File.createTempFile("temporary", "tmp")
    val tmpPath = tmpFile.getAbsolutePath
    block(tmpPath)
    tmpFile.delete
  }
  
  def noOutput(block: => Unit){
    if(DisableStdout){
      val original = System.out
      val nullStream = new PrintStream(new java.io.OutputStream { def write(b: Int) {} })
      System.setOut(nullStream)
      block
      System.setOut(original)
    }
    else block
  }
  
  def train(modelFile: String, trainingFile: String) = {
      /* example params:
       * -train MQ2008/Fold1/train.txt -test MQ2008/Fold1/test.txt -validate MQ2008/Fold1/vali.txt 
       * -ranker 6 -metric2t NDCG@10 -metric2T ERR@10 -save mymodel.txt
       */
      Evaluator.main(Seq(
          "-train", trainingFile, 
          "-tree", "10",
          "-validate", trainingFile, // specify training as validation, so we get early stopping!
          "-ranker", "6",
          "-metric2t", "NDCG@10",
          "-save", modelFile).toArray)
  }
  
  def rank(modelFile: String, dataFile: String, outFile: String, metric: String = "ERR@10") = noOutput {
    Evaluator.main(Seq(
        "-load", modelFile, 
        "-rank", dataFile, 
        "-score", outFile,
        "-metric2T", metric).toArray)
  }
  

  def toLetorFormat(data: Iterable[RankEntry]): String = {
    /* LETOR format: 
     * Each row is a query-document pair. 
     * The first column is relevance label of this pair, the second column is query id, the following columns
     * are features, and the end of the row is comment about the pair, including id of the document. */  
    val lines = data.map { case RankEntry(queryId, docID, relevanceScore, features) =>
      val featurePairs = features.toList.map { case (featureName, value) => featureName + ":" + value }
      s"$relevanceScore qid:$queryId ${featurePairs.mkString(" ")} # docId:$docID"
    }
    lines.mkString("\n")
  }
}




object TestRankLibWrapper extends App {
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
    ranker.retrain(data, tmp)
  }
  println("ranking..")
  println(ranker.rank(data))
}