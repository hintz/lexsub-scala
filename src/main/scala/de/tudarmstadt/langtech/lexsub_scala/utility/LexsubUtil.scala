package de.tudarmstadt.langtech.lexsub_scala.utility

import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalReader
import de.tudarmstadt.langtech.scala_utilities.io
import scala.util.Try
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.LexSubProcessing

object LexsubUtil {
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file */
  def preprocessSemEval(folder: String, datafile: String, goldfile: String)
                       (implicit preprocessing: LexSubProcessing): Seq[LexSubInstance] = {
     val cachefile = "cache/%s.ser".format((folder + "-" + datafile).replaceAll("""[\/\.]+""","-"))
     io.lazySerialized(cachefile){
      System.err.println("Cachefile does not exist, preprocessing SemEval data..")
      val plainData = new SemEvalReader(folder, datafile, goldfile).items
      preprocessing.parseSemEval(plainData)
    }
  }
  
  /** Loads and preprocesses SemEval data and caches it in a temporary file
   *  (default naming convention)
   */
  def preprocessSemEval(germevalFolder: String, filename: String)
                       (implicit preprocessing: LexSubProcessing): Seq[LexSubInstance] = 
    preprocessSemEval(germevalFolder, filename + ".xml", filename + ".gold")

}