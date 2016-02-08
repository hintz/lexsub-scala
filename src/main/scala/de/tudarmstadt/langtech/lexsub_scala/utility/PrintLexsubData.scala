package de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance

object PrintLexsubData {

  def main(args: Array[String]){
    val Array(infile) = args
    println("Printing lexsub data in " + infile)
    val data: Seq[LexSubInstance] = io.deserialize(infile)
    data foreach println
  }
}