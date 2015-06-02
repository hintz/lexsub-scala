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
  
  def getSematicRelationPath(orthFormStart: String, orthFormTarget: String, maxDepth: Int = 15): Seq[String] = {
    // Perfrom BFS search through all relations
    val synsetStart = gn.getSynsets(orthFormStart).asScala
    val synsetTarget = gn.getSynsets(orthFormTarget).asScala
    
    val visited = new collection.mutable.HashMap[Synset, List[String]]
    var frontier = Set(synsetStart.map(x => (x, List.empty[String])) :_*)

    for (depth <- 0 to maxDepth) {
      val next =
        for (
          (synset, path) <- frontier;
          rel <- ConRel.values;
          related <- synset.getRelatedSynsets(rel).asScala if !visited.contains(related);
          val step = synset.getId + "_" + rel.toString) 
            yield (related, step :: path)
      visited ++= next
      frontier = next
      
      if(synsetTarget.exists(visited.contains)) 
        return synsetTarget.collectFirst { case s if visited contains s => visited(s) }.get
    }
    Seq.empty
  }

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
      val path = getSematicRelationPath(target, substitute)
      val pathLength = if(path.nonEmpty) path.length else -1
      val row = Seq(item.id, target, substitute, nTargetSynsets, nSubstituteSynsets, rel, pathLength, path.mkString("->"))
      println(row.mkString("\t"))
    }
  }
}