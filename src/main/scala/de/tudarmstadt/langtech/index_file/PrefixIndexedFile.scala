package de.tudarmstadt.langtech.index_file

import java.io.RandomAccessFile
import scala.collection.mutable.HashMap
import java.io.File
import scala.Left
import scala.Right
import de.tudarmstadt.langtech.lexsub_scala.utility.io

trait Index extends java.io.Serializable {
  def search(prefix: String): (Long, Long)
}

/*
  case class SplitTree(val c: Char, val begin: Long,
      val smaller: Either[SplitTree, Long], 
      val bigger: Either[SplitTree, Long],
      val next: Option[SplitTree]) extends Index with Serializable
  {
    def end: Long = bigger match {
        case Left(tree) => tree.begin
        case Right(end) => end
    }
    
    def search(prefix: String) = search(prefix.toSeq)
    def search(prefix: Seq[Char]): (Long, Long) = prefix match {
      case Seq() => throw new IllegalStateException
      
      case Seq(`c`) => (begin, end)
      
      case Seq(x, xs@_*) if x < c => smaller match {
        case Left(tree) => tree.search(prefix)
        case Right(leftmost) => (leftmost, begin)
      }
      
      case Seq(x, xs@_*) if x > c => bigger match {
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
  */


case class FixedSizePrefixIndex(index: Map[String, Long], end: Long) extends Index with Serializable {
  val byLength = index.keys.groupBy(_.length).mapValues(_.toVector.sorted).map(identity)
  val maxLength = byLength.keySet.max
  def search(prefix: String): (Long, Long) = {
    val actualPrefix = prefix.take(maxLength) // cap prefix
    val prefixLength = actualPrefix.length // only consider given length
    val subindexKeys = byLength(prefixLength) // extract keys for given length
    val firstLarger = subindexKeys.indexWhere(_ > actualPrefix) // find first key that's bigger
    val result: (Long, Long) = firstLarger match {
      case i if i >= 0 => (index(subindexKeys(i - 1)), index(subindexKeys(i))) // default case: between previous and entry
      case 0           => (0, index(subindexKeys.head)) // must be between beginning and first entry
      case -1          => (index(subindexKeys.last), end) // must be between last entry and end
    }
    result
  }
}

class PrefixIndexedFile(val path: String) {

  val file = new RandomAccessFile(path, "r")

  val index: Index = {
    val indexpath = path + ".index"
    if (!new File(indexpath).exists) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val index: Index = generatedFixedPrefixIndex(3)
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(indexpath)).writeObject(index)
      index
    } else {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(indexpath))
      s.readObject.asInstanceOf[Index]
    }
  }

  def search(prefix: String): Iterator[String] = {
    val (begin, end) = index.search(prefix)
    file.seek(begin)
    val lines = for (line <- Iterator.continually(file.readLine).takeWhile(line => file.getFilePointer <= end))
      yield line
    val cleaned = lines.dropWhile(!_.startsWith(prefix)).takeWhile(_.startsWith(prefix))
    cleaned
  }

  private def generatedFixedPrefixIndex(prefixLength: Int): FixedSizePrefixIndex = {

    val fileLength = file.length
    val fullPrefexIndex = new HashMap[String, Long] // prefix -> beginning
    file.seek(0) // go to beginning
    for (line <- Iterator.continually(file.readLine).takeWhile(_ != null)) {
      for (i <- 0 to prefixLength) {
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), file.getFilePointer - line.length - 1)
      }
    }

    new FixedSizePrefixIndex(fullPrefexIndex.toMap, fileLength)
  }

  /*
  private def generateIndex(levels: Seq[Int]): SplitTree = {
    
    val fileLength = file.length
    val maxPrefixLen = levels.length
    val fullPrefexIndex = new HashMap[Seq[Char], Long] // charSeqence -> beginning
    val children = new HashMap[Seq[Char], SplitTree]
    
    for(line <- Iterator.continually(file.readLine).takeWhile(_ != null)) {
      
      
      levels.indices.map { i =>         
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), file.getFilePointer - line.length - 1)
      }
    }
    
    val byLevel = levels.indices.map { i => 
        fullPrefexIndex.collect { case (seq, v) if seq.length == i + 1 => (v, seq) }.toSeq.sortBy(_._1)
    }
    
    def build(level: Seq[(Long, Seq[Char])]): SplitTree = {
      // should never be called on empty list
      if(level.isEmpty) throw new IllegalStateException
      val (left, middleRight) = level.splitAt(level.length / 2)
      val (middle, right) = middleRight.splitAt(1)
      val beginPos = middle.head._1
      val prefix = middle.head._2
      val parent = prefix.init
      val c = prefix.last
      
      val leftTree = if(left.nonEmpty) Left(build(left)) else Right(0l)
      val rightTree = if(right.nonEmpty) Left(build(right)) else Right(fileLength)
      val result = SplitTree(c, beginPos, leftTree, rightTree, children.get(prefix))
      children(parent) = result
      result
    }
    
    //for((level, i) <- byLevel.zipWithIndex.reverse){
    //  build(level)
    //}
    
    //val result = children(Seq.empty[Character])

    
    //result
    null
  }
  */

}

object Test extends App {
  val file = new PrefixIndexedFile("F:/AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt")
  //new PrefixIndexedFile("/Volumes/AIPHES_HDD/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub")
  file.search("wo") foreach println
}