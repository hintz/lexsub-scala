package de.tudarmstadt.langtech.lexsub_scala.twsi.setup

import de.tudarmstadt.langtech.scala_utilities.{ io, strings }
import de.tudarmstadt.ukp.lmf.transform.DBConfig
import de.tudarmstadt.ukp.lmf.api.Uby
import scala.collection.JavaConverters._
import de.tudarmstadt.ukp.lmf.model.core.Lexicon

object CreateUBYCandidates extends App {

  val targets = io.lines("resources/twsi/targets-pos.txt").map(strings.splitAssign('\t')).toList
  val ubyDatabase = "../AIPHES_Data/UBY/ubymedium070"

  val uby = new Uby(new DBConfig(
    "jdbc:h2:file:" + ubyDatabase,
    "org.h2.Driver", "h2", "sa", "", false))
  
  val lexicons = List("WordNet", "OmegaWiki_eng", "OntoWiktionaryEN", "WiktionaryEN") // or: uby.getLexiconNames.asScala

  System.out.println("Creating candidates from UBY database with the following lexicons: " + lexicons);

  for (lexiconName <- lexicons; val lexicon = uby.getLexiconByName(lexiconName)) {
    println("Processing lexicon " + lexiconName + "..")
    val outputFile = "resources/candidates/twsi/uby_" + lexiconName + ".tsv"
    val lines = for ((lemma, pos) <- targets; (expansion, label) <- candidatesForLexicon(lemma, lexicon))
      yield Seq(lemma, pos, expansion.replace('\n', ' '), label).mkString("\t")
    if (lines.length > 1) // UBY might contain some useless dictionaries (wrong language, etc.)
      io.write(outputFile, lines.sorted.distinct.mkString("\n"))
  }

  println("Done.")
  
  
  def candidatesForLexicon(lemma: String, lexicon: Lexicon = null) = {
    List(
      synonymsForLexicon(lemma, lexicon),
      senseRelationsForLexicon(lemma, lexicon),
      synsetRelationsForLexicon(lemma, lexicon)
      ).flatten
  }

  def senseRelationsForLexicon(lemma: String, lexicon: Lexicon = null) = {
    for (
      entry <- uby.getLexicalEntries(lemma, lexicon).asScala; 
      sense <- entry.getSenses.asScala;
      rel <- sense.getSenseRelations.asScala; val lexiconName = lexicon.getName; val relName = rel.getRelName;
      val relation = lexiconName + "_" + relName.replace(' ', '_');
      val target = rel.getTarget if target != null;
      targetForm <- target.getLexicalEntry.getLemma.getFormRepresentations.asScala;
      val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
    ) yield (targetLemma, relation)
  }
  
  def synonymsForLexicon(lemma: String, lexicon: Lexicon = null) = {
    for (
      entry <- uby.getLexicalEntries(lemma, lexicon).asScala; 
      synset <- entry.getSynsets.asScala;
      val synonymous = synset.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
      targetForm <- synonymous;
      val lexiconName = lexicon.getName;
      val relation = lexiconName + "_synset";
      val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
      ) yield (targetLemma, relation)
  }

  def synsetRelationsForLexicon(lemma: String, lexicon: Lexicon = null) = {
    for (
      entry <- uby.getLexicalEntries(lemma, lexicon).asScala; 
      synset <- entry.getSynsets.asScala;
      val synonymous = synset.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
      rel <- synset.getSynsetRelations.asScala; val lexiconName = lexicon.getName; val relName = rel.getRelName;
      val relation = lexiconName + "_" + relName.replace(' ', '_');
      val target = rel.getTarget if target != null;
      targetForm <- target.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
      val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
    ) yield (targetLemma, relation)
  }
}