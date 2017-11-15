package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015.setup

import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalGold
import de.tudarmstadt.langtech.lexsub_scala.utility.GermaNetUtils

object InspectRelations extends App {
	val gn = new GermaNet("GermaNet/GN_V90/GN_V90_XML")
  //val gn = new GermaNet("GermaNet/GN_V80/GN_V80_XML")
  val utils = new GermaNetUtils(gn)
  import utils._
  
  //val ss: Seq[Synset] = gn.getSynsets("Erleichterung").asScala
  //val sample = ss.head
  //println(sample)
  
  getSemanticRelations("essen")
  getSemanticRelations("Ackerwalze")
  
  val gold = new SemEvalGold("Tasks/GermEval2015/train-dataset.gold")
  
  for(item <- gold.items){
    val target = item.targetWord
    val nTargetSynsets = gn.getSynsets(target).size
    val relations = getSemanticRelations(target)
    for(substitute <- item.substitutionWords){
      val nSubstituteSynsets = gn.getSynsets(substitute).size
      val rel = relations.get(substitute).map(x => x.mkString(", ")).getOrElse("-")
      val path = getSematicRelationPath(target, substitute)
      val pathLength = if(path.nonEmpty) path.length else -1
      val row = Seq(item.id, target, substitute, nTargetSynsets, nSubstituteSynsets, rel, pathLength, path.mkString("->"))
      println(row.mkString("\t"))
    }
  }
}
