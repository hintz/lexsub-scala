package de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.features.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions

object PrintFeaturizedData {

  type Featurized = (Substitutions, Vector[Seq[Feature]])

  def pp(lsi: LexSubInstance): String = {
    s"[${lsi.id}] ${lsi.head.lemma}: " + lsi.getGold.sentence.sentence
  }

  def prettyPrint(data: Iterable[Featurized]) {
    data.foreach {
      case (item, featureVectors) =>
        println(pp(item.lexSubInstance))
        println(s"\twith ${item.candidates.length} candidates")
        item.asItems.sortBy(-_.relevance.get).zip(featureVectors).foreach {
          case (cand, feats) =>
            val line = s"\t${cand.relevance.get}\t${cand.substitution}\t${feats.mkString(" ")}"
            println(line)
        }
    }
  }

  def main(args: Array[String]) {
    val Array(infile) = args
    println("Printing lexsub data in " + infile)
    val data: Iterable[Featurized] = io.deserialize(infile)
    prettyPrint(data)
  }
}