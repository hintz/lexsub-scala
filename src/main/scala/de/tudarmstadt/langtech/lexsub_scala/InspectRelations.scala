package de.tudarmstadt.langtech.lexsub_scala

import de.tuebingen.uni.sfs.germanet.api.GermaNet
import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.ConRel
import de.tuebingen.uni.sfs.germanet.api.Synset

object InspectRelations extends App {
	val gn = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  //val gn = new GermaNet("AIPHES_Data/GermaNet/GN_V80/GN_V80_XML")
  val ss: Seq[Synset] = gn.getSynsets("Erleichterung").asScala
  val sample = ss.head
  println(sample)
  
  
  def getSemanticRelations(orthForm: String){
    val synsets = gn.getSynsets(orthForm).asScala
    
    val synonymous = for(
        synset <- synsets;
        lexUnit <- synset.getLexUnits.asScala if !lexUnit.isArtificial;
        word <- lexUnit.getOrthForms.asScala) yield ("synonym", word)
    
    val related = for(
        synset <- synsets; 
        rel <- ConRel.values;
        related <- synset.getRelatedSynsets(rel).asScala;
        relatedLex <- related.getLexUnits.asScala if !relatedLex.isArtificial;
        relatedOrthForm <- relatedLex.getOrthForms.asScala
        ) yield (rel.toString, relatedOrthForm) 
        
        
   val result = synonymous.distinct ++ related.distinct
      
    
    println(result)  

  }
  
  getSemanticRelations("essen")
  
  sample.getRelatedSynsets(ConRel.has_hypernym)
  
  val lexunits = sample.getLexUnits.asScala
  
  val lexsample = lexunits.head
  
  lexsample.getSynset
  
  
  
  
  //sample.getRelatedSynsets(ConRel.has_hyponym).asScala foreach println
  sample.getLexUnits.asScala.head.getSynonyms.asScala foreach println
  
  
  
  
  
  //sample.getRelatedSynsets.asScala foreach println
  
}