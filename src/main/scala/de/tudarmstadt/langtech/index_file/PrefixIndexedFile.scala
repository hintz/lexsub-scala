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
class PrefixIndexedFile(val path: String, val prefixLength: Int = 5) {

  val file = new RandomAccessFile(path, "r")

  /** Fixes behavior of RandomAccessFile::readLine (proper UTF-8 reading!)
   *  Leaves the filepointer at the beginning of the next line! */
  def readline: String = {
    var readData = false
    val bytes = Iterator.continually(file.read).takeWhile(_ match {
      case -1   => false
      case '\n' => readData = true; false
      case '\r' =>
        readData = true;
        // swallow another character, if its \n
        val cur = file.getFilePointer
        if (file.read != '\n') file.seek(cur)
        false
      case byte => 
        readData = true; 
        true
    })
    val byteArray = bytes.map(_.toByte).toArray
    val string = if(readData) new String(byteArray) else null
    string
  }

  val index: PrefixFileIndex = {
    val indexpath = path + ".index"
    if (!new File(indexpath).exists) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val index: PrefixFileIndex = generatedFixedPrefixIndex
      System.err.println("Writing index " + indexpath)
      new java.io.ObjectOutputStream(new java.io.FileOutputStream(indexpath)).writeObject(index)
      index
    } else {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(indexpath))
      s.readObject.asInstanceOf[PrefixFileIndex]
    }
  }
  

  def search(prefix: String): List[String] = {
    //val file = if(lazyRead) new RandomAccessFile(path, "r") else this.file
    file synchronized {
      val (begin, end) = index.search(prefix)
      file.seek(begin)
      val lines = for (line <- Iterator.continually(readline).takeWhile(line => file.getFilePointer <= end))
        yield line
      
      // profiling..
      /*
      val foo = lines.toList
      println("Searching with prefix '%s', resulting in %d lines".format(prefix, foo.length))
      val dropped = foo.dropWhile(!_.startsWith(prefix))
      println(".. dropped left %d lines".format(foo.length - dropped.length))
      val taken = dropped.takeWhile(_.startsWith(prefix))
      println(".. dropped right %d lines".format(dropped.length - taken.length))
      return taken
      */
        
      val cleaned = lines.dropWhile(!_.startsWith(prefix)).takeWhile(_.startsWith(prefix))
      cleaned.toList
    }
  }

  private def generatedFixedPrefixIndex: FixedSizePrefixIndex = {
    val fileLength = file.length
    val fullPrefexIndex = new HashMap[String, Long] // prefix -> beginning
    
    // go to beginning
    var lineBegin = 0l
    file.seek(lineBegin) 
    
    // reporting
    var lastLine = -1l
    val ReportEachMillis = 5000
    var lastReport = System.currentTimeMillis
    
    for (line <- Iterator.continually(readline).takeWhile(_ != null)) {
      
      assert(lineBegin > lastLine)
      
      // reporting
      val now = System.currentTimeMillis()
      if(now - lastReport >= ReportEachMillis){
        val percentage = lineBegin.toFloat * 100 / fileLength
        System.err.println("Indexing %.1f%%".format(percentage))
        lastReport = now
      }

      for (i <- 0 to prefixLength) {
        fullPrefexIndex.getOrElseUpdate(line.take(i + 1), lineBegin)
      }
      
      lastLine = lineBegin
      lineBegin = file.getFilePointer
    }
    new FixedSizePrefixIndex(fullPrefexIndex.toMap, fileLength)
  }
}

object Test extends App {
  //val file = new PrefixIndexedFile("F:/AIPHES_Data/LexSub/coocs/germeval_coocs_truecase.txt")
  val file = new PrefixIndexedFile("../lexsub-gpl/AIPHES_Data/DT/de70M_mate_lemma/de70M_parsed_lemmatized_LMI_s0.0_w2_f2_wf0_wpfmax1000_wpfmin2_p1000_simsortlimit200_lexsub")
  file.search("würgen") foreach println
  file.search("wüten") foreach println
  file.search("wissend") foreach println
}