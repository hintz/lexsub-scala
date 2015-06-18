package de.tudarmstadt.langtech.lexsub_scala.germanet

import de.tuebingen.uni.sfs.germanet.api.GermaNet
import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import de.tuebingen.uni.sfs.germanet.api.ConRel
import de.tuebingen.uni.sfs.germanet.api.Synset
import de.tuebingen.uni.sfs.germanet.api.WordCategory
import de.tuebingen.uni.sfs.germanet.api._

class GermaNetUtils(val gn: GermaNet) {
  
  // defines mapping between WordCategory and String representation
  val posMapper = (wc: WordCategory) => wc.toString.substring(0, 1).toLowerCase
  val posTranslation = WordCategory.values.zip(WordCategory.values.map(posMapper))
  val posMap = posTranslation.toMap
  val posBackmap = posTranslation.map(_.swap).toMap

  /** Translates POS back-and-forth between LexSub's string representation and internal GermaNet representation */
  def translatePos(s: String): Option[WordCategory] = posBackmap.get(s)
  def translatePos(pos: WordCategory): String = posMap(pos)
  
  /* TODO: port?
  def getTransRelated(syn: Synset, rel: ConRel): Future[Seq[Synset]] = {
    Future {
      syn.getTransRelatedSynsets(rel).asScala.flatMap(_.asScala)
    }
  } */
  
  def getTransRelated(syn: Synset, rel: ConRel): Seq[Seq[Synset]] = {
    // the GermaNet code ends up in endless loops, just ignore those cases!
    val tryRelations = runWithTimeout(100)(syn.getTransRelatedSynsets(rel).asScala.map(_.asScala))
    tryRelations.getOrElse(Seq.empty)
  }
  
 def runWithTimeout[T](timeoutMs: Long)(f: => T) : Option[T] = {
    actors.Futures.awaitAll(timeoutMs, actors.Futures.future(f)).head.asInstanceOf[Option[T]]
  }
  
  // entries in GermaNet which seem to cause problems!
  // val Blacklist = Set("Acklins Island", "Amerika", "Andros Island")
  
  def getSemanticRelations(orthForm: String, pos: WordCategory): Map[String, Seq[String]] = getSemanticRelations(orthForm, Some(pos))
  def getSemanticRelations(orthForm: String, pos: Option[WordCategory] = None): Map[String, Seq[String]] = {
    println("getSemanticRelations(%s)".format(orthForm))
    
    val synsets = (if(pos.isDefined) gn.getSynsets(orthForm, pos.get) else gn.getSynsets(orthForm)).asScala

    val synonymous = for (
      synset <- synsets;
      lexUnit <- synset.getLexUnits.asScala if !lexUnit.isArtificial;
      word <- lexUnit.getOrthForms.asScala if word != orthForm
    ) yield (word, "synonym")

    val related = for (
      synset <- synsets;
      rel <- ConRel.values;
      (transrelated, idx) <- getTransRelated(synset, rel).zipWithIndex;
      related <- transrelated;
      relatedLex <- related.getLexUnits.asScala if !relatedLex.isArtificial;
      relatedOrthForm <- relatedLex.getOrthForms.asScala if relatedOrthForm != orthForm
    ) yield (relatedOrthForm, rel.toString.dropWhile(_ != '_').tail + "_" + idx)

    val both = synonymous.distinct ++ related.distinct
    val result = both.groupBy(_._1).mapValues(_.map(_._2))
    result
  }

  def getSematicRelationPath(orthFormStart: String, orthFormTarget: String, maxDepth: Int = 15): Seq[String] = {
    // Perfrom BFS search through all relations
    val synsetStart = gn.getSynsets(orthFormStart).asScala
    val synsetTarget = gn.getSynsets(orthFormTarget).asScala

    val visited = new collection.mutable.HashMap[Synset, List[String]]
    var frontier = Set(synsetStart.map(x => (x, List.empty[String])): _*)

    for (depth <- 0 to maxDepth) {
      val next =
        for (
          (synset, path) <- frontier;
          rel <- ConRel.values;
          related <- synset.getRelatedSynsets(rel).asScala if !visited.contains(related);
          val step = synset.getId + "_" + rel.toString
        ) yield (related, step :: path)
      visited ++= next
      frontier = next

      if (synsetTarget.exists(visited.contains))
        return synsetTarget.collectFirst { case s if visited contains s => visited(s) }.get
    }
    Seq.empty
  }
}
