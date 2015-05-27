package de.tudarmstadt.langtech.lexsub_scala

import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tuebingen.uni.sfs.germanet.api.ConRel
import de.tuebingen.uni.sfs.germanet.api.Synset
import de.tudarmstadt.langtech.lexsub_scala.germeval.GermEvalGold

object InspectRelations extends App {
	val gn = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  //val gn = new GermaNet("AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  
  //val ss: Seq[Synset] = gn.getSynsets("Erleichterung").asScala
  //val sample = ss.head
  //println(sample)
  
  def getSemanticRelations(orthForm: String): Map[String, Seq[String]] = {
    val synsets = gn.getSynsets(orthForm).asScala
    
    val synonymous = for(
        synset <- synsets;
        lexUnit <- synset.getLexUnits.asScala if !lexUnit.isArtificial;
        word <- lexUnit.getOrthForms.asScala) yield (word, "synonym")
    
    val related = for(
        synset <- synsets; 
        rel <- ConRel.values;
        transrelated <- synset.getTransRelatedSynsets(rel).asScala.map(_.asScala);
        (related, idx) <- transrelated.zipWithIndex;
        relatedLex <- related.getLexUnits.asScala if !relatedLex.isArtificial;
        relatedOrthForm <- relatedLex.getOrthForms.asScala
        ) yield (relatedOrthForm, rel.toString.dropWhile(_ != '_').tail + "_" + idx)
        
    val both = synonymous.distinct ++ related.distinct
    val result = both.groupBy(_._1).mapValues(_.map(_._2))
    result
  }
  
  getSemanticRelations("essen")
  
  val gold = new GermEvalGold("AIPHES_Data/GermEval2015/train-dataset.gold")
  
  for(item <- gold.items){
    val target = item.targetWord
    val nTargetSynsets = gn.getSynsets(target).size
    val relations = getSemanticRelations(target)
    for(substitute <- item.substitutionWords){
      val nSubstituteSynsets = gn.getSynsets(substitute).size
      val rel = relations.get(substitute).map(x => x.mkString(", ")).getOrElse("-")
      val row = Seq(item.id, target, substitute, nTargetSynsets, nSubstituteSynsets, rel)
      println(row.mkString("\t"))
    }
  }
  
}