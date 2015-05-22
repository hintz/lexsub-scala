package de.tudarmstadt.langtech.lexsub_scala

import java.io.RandomAccessFile
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import scala.collection.mutable.HashMap

class PrefixIndexedFile(val path: String) {
  
  type T = Char
  case class SplitTree(val c: T, val begin: Long,
      val smaller: Either[SplitTree, Long], 
      val bigger: Either[SplitTree, Long],
      val next: Option[SplitTree])
  {
    def end: Long = bigger match {
        case Left(tree) => tree.begin
        case Right(end) => end
    }
    
    def search(prefix: Seq[T]): (Long, Long) = prefix match {
      case Seq() => throw new IllegalStateException
      case Seq(`c`) => (begin, end)
      
      case x :: xs if x < c => smaller match {
        case Left(tree) => tree.search(prefix)
        case Right(leftmost) => (leftmost, begin)
      }
      
      case x :: xs if x > c => bigger match {
        case Left(tree) => tree.search(prefix)
        case Right(rightmost) => (begin, rightmost)
      }
      
      // found the area, recurse to next prefix char
      case `c` :: xs => next match {
        case Some(tree) => tree.search(xs)
        case None => (begin, end)
      }
    }
  }

  val file = new RandomAccessFile(path, "r")
  val index: SplitTree = {
    val indexpath = path + ".index"
    if (!io.exists(indexpath)) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val index = generateIndex(Seq(10, 10))
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(indexpath)).writeObject(index)
      index
    }
    else {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(indexpath))
      s.readObject.asInstanceOf[SplitTree]
    }
  }
  
  def search(prefix: String) = index.search(prefix)
 
  private def generateIndex(levels: Seq[Int]): SplitTree = {
    val maxPrefixLen = levels.length
    val fullPrefexIndex = new HashMap[Seq[T], Long] // charSeqence -> beginning
    
    for(line <- Iterator.continually(file.readLine).takeWhile(_ != null)) {
      levels.indices.map { i => 
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), file.getFilePointer - line.length - 1)
      }
    }
    
    val byLevel = levels.indices.map { i => 
        fullPrefexIndex.collect { case (seq, v) if seq.length == i + 1 => (v, seq) }.toSeq.sortBy(_._1)
    }
    
    println(fullPrefexIndex)
    
    def build(level: Seq[(Long, Seq[T])], children: Map[Seq[T], SplitTree]): SplitTree = level match {
      
      case Seq() => throw new IllegalStateException
      
      // only a single element
      case Seq((beginPos, prefix)) =>
        val parent = prefix.init
        val c = prefix.last
        SplitTree(c, beginPos, Right(0), Right(0), children.get(prefix))
        
      case _ => 
        val (left, middleRight) = level.splitAt(level.length / 2)
        val (middle, right) = middleRight.splitAt(1)
        val beginPos = middle.head._1
        val prefix = middle.head._2
        val parent = prefix.init
        val c = prefix.last
        
        ///SplitTree(c, beginPos, build(left, children), build(right, children), children.get(prefix))


      ???
    }
    
    for((level, i) <- byLevel.zipWithIndex){
      println(i)
      println(level)
      
      val (left, middleRight) = level.splitAt(level.length / 2)
      val (middle, right) = middleRight.splitAt(1)
      
      println(left, middle, right)
      
    }
    
    def getLineBeginning(pos: Long): String = {
      file.seek(pos)
      file.readLine()
      file.getFilePointer().toString()
    }
    
    val maxlen = file.length
    var split = maxlen / 2
    
    println(getLineBeginning(split))
    ???
  }
  
  
}


object Test extends App {
  new PrefixIndexedFile("/Volumes/AIPHES_HDD/AIPHES_Data/coocs/deu_news_10M/germeval_coocs.txt")
  //new PrefixIndexedFile("/Volumes/AIPHES_HDD/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub")
}