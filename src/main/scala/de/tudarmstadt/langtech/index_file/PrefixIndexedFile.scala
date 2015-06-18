package de.tudarmstadt.langtech.index_file

import java.io.RandomAccessFile
import scala.collection.mutable.HashMap
import java.io.File
import scala.Left
import scala.Right
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import java.nio.ByteBuffer

/**
 * A reader for a *SORTED* textfile encoded in UTF-8.
 * Retrieves subset of lines for a given prefix
 */
class PrefixIndexedFile(val path: String, val prefixLength: Int = 4) {

  val file = new RandomAccessFile(path, "r")

  /** Fixes behaviour of RandomAccessFile::readLine */
  def readline: String = {
    var readData = false
    val bytes = Iterator.continually(file.read).takeWhile(_ match {
      case -1   => false
      case '\n' => readData = true; false
      case '\r' =>
        readData = true;
        val cur = file.getFilePointer
        if (file.read != '\n') file.seek(cur)
        false
      case byte => 
        readData = true; 
        true
    })
    val byteArray = bytes.map(_.toByte).toArray
    return if(readData) new String(byteArray) else null
  }

  val index: PrefixFileIndex = {
    val indexpath = path + ".index"
    if (!new File(indexpath).exists) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val index: PrefixFileIndex = generatedFixedPrefixIndex
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(indexpath)).writeObject(index)
      index
    } else {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(indexpath))
      s.readObject.asInstanceOf[PrefixFileIndex]
    }
  }

  def search(prefix: String): Iterator[String] = {
    val (begin, end) = index.search(prefix)
    file.seek(begin)
    val lines = for (line <- Iterator.continually(readline).takeWhile(line => file.getFilePointer <= end))
      yield line
    val cleaned = lines.dropWhile(!_.startsWith(prefix)).takeWhile(_.startsWith(prefix))
    cleaned
  }

  private def generatedFixedPrefixIndex: FixedSizePrefixIndex = {
    val fileLength = file.length
    val fullPrefexIndex = new HashMap[String, Long] // prefix -> beginning
    file.seek(0) // go to beginning
    
    var lastLine = -1l
    val reportEachPercent = 0.1
    var lastPercentage = Float.MinValue
    for (line <- Iterator.continually(readline).takeWhile(_ != null)) {
      val lineBegin = file.getFilePointer - line.length - 1
      assert(lineBegin > lastLine)
      val percentage = lineBegin.toFloat * 100 / fileLength
      if(percentage - lastPercentage >= reportEachPercent){
        System.err.println("Indexing %.1f%%".format(percentage))
        lastPercentage = percentage
      }
      for (i <- 0 to prefixLength) {
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), lineBegin)
      }
      lastLine = lineBegin
    }
    new FixedSizePrefixIndex(fullPrefexIndex.toMap, fileLength)
  }
}

object Test extends App {
  //val file = new PrefixIndexedFile("F:/AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt")
  val file = new PrefixIndexedFile("../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub")
  file.search("wüten") foreach println
}