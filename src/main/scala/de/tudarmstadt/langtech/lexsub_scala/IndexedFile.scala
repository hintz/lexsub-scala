package de.tudarmstadt.langtech.lexsub_scala

import java.io.RandomAccessFile
import java.io.File
import scala.util.matching.Regex
import java.io.BufferedReader
import java.io.FileReader
import java.io.InputStreamReader
import java.io.FileInputStream
import scala.collection.mutable.ListBuffer
import de.tudarmstadt.langtech.lexsub_scala.utility.io

/**
 * A convenience file reader which uses a line index.
 * Each line is parsed as [key] [separator] [rest of line]
 * where all equal [key]s should be adjacent in the file.
 * These consecutive lines can then be retrieved efficiently by [key].
 *
 * If the index file does not exist, it is generated and written to the file system.
 * 
 * @author gerold
 */
class IndexedFile(val path: String, val separator: String = "\t") {

  val file = new RandomAccessFile(path, "r")
  val index = {
    val indexpath = IndexedFile.getIndexPath(path)
    if (!io.exists(indexpath)) {
      System.err.println("Index file " + indexpath + " does not exist. Creating index..")
      val lines = IndexedFile.generateIndex(path)
      io.write(indexpath, lines.mkString("\n"))
    }
    io.lines(indexpath).map(_.split(separator, 1).toSeq).collect {
      case Seq(key, from, to) => (key, (from.toLong, to.toLong))
    }.toMap
  }

  /** Retrieves exact key */
  def lookup(key: String): Option[String] = {
    val (from, to) = index.getOrElse(key, return None)
    file seek from
    val buffer = new Array[Byte]((to - from).toInt)
    file readFully buffer
    Some(new String(buffer))
  }

  /** Returns all keys which match the regex query */
  def search(regex: String): Iterable[String] = {
    val r = regex.r
    index.keys.filter(s => r.findFirstMatchIn(s).isDefined)
  }

  /** Retrieves a concatenated result of everything that matches regex */
  def like(regex: String, seperator: String = "") =
    search(regex).flatMap(lookup).mkString(seperator)

}

object IndexedFile {

  def getIndexPath(path: String) = path + ".index"
  def generateIndex(path: String, separator: Char = '\t'): List[String] = {
    def extractIndex(line: String) = line.split(separator)(0)
    val file = new RandomAccessFile(path, "r")
    val index = new ListBuffer[String]

    var line = file.readLine
    var currentBegin: Long = 0
    var currentEnd: Long = 0
    var currentIndex: String = extractIndex(line)

    while (line ne null) {
      val indexpart = extractIndex(line)
      if (indexpart != currentIndex) {
        val indexline = currentIndex + separator + currentBegin + separator + currentEnd
        index += indexline
        currentBegin = currentEnd
        currentIndex = indexpart
      }
      currentEnd = file.getFilePointer
      line = file.readLine
    }
    file.close
    index.toList
  }
}