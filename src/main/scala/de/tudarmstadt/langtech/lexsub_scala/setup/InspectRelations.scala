package de.tudarmstadt.langtech.lexsub_scala.setup

import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalGold
import de.tudarmstadt.langtech.lexsub_scala.germanet.GermaNetUtils

object InspectRelations extends App {
	val gn = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  //val gn = new GermaNet("AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  val utils = new GermaNetUtils(gn)
  import utils._
  
  //val ss: Seq[Synset] = gn.getSynsets("Erleichterung").asScala
  //val sample = ss.head
  //println(sample)
 
  
  getSemanticRelations("essen")
  getSemanticRelations("Ackerwalze")
  
  val gold = new GermEvalGold("AIPHES_Data/GermEval2015/train-dataset.gold")
  
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