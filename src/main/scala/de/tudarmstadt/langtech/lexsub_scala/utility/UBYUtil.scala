package de.tudarmstadt.langtech.lexsub_scala.utility

import de.tudarmstadt.langtech.scala_utilities.{ io, strings }
import de.tudarmstadt.ukp.lmf.transform.DBConfig
import de.tudarmstadt.ukp.lmf.api.Uby
import scala.collection.JavaConverters._
import de.tudarmstadt.ukp.lmf.model.core.Lexicon
import de.tudarmstadt.ukp.lmf.model.enums.EPartOfSpeech

class UBYUtil(ubyDatabase: String) {

  val uby = new Uby(new DBConfig(
    "jdbc:h2:file:" + ubyDatabase,
    "org.h2.Driver", "h2", "sa", "", false))

  /** creates candidate expansion files based on UBY */
  def createCandidates(targets_with_pos_file: String, output_dir: String) {
    val lexicons = List("WordNet", "OmegaWiki_eng", "OntoWiktionaryEN", "WiktionaryEN") // or: uby.getLexiconNames.asScala

    val targets = io.lines(targets_with_pos_file).map(strings.splitAssign('\t')).toList

    println("Creating UBY candidates from input %s writing to output directory %s".format(targets_with_pos_file, output_dir))
    println("Creating candidates from UBY database with the following lexicons: " + lexicons)

    for (lexiconName <- lexicons; val lexicon = uby.getLexiconByName(lexiconName)) {
      println("Processing lexicon " + lexiconName + "..")
      val outputFile = output_dir + "/uby_" + lexiconName + ".tsv"
      val lines = for ((lemma, pos) <- targets; (expansion, label) <- candidatesForLexicon(lemma, pos, lexicon))
        yield Seq(lemma, pos, expansion.replace('\n', ' '), label).mkString("\t")
      if (lines.length > 1) // UBY might contain some useless dictionaries (wrong language, etc.)
        io.write(outputFile, lines.sorted.distinct.mkString("\n"))
    }
    println("Done.")

  }

  def candidatesForLexicon(lemma: String, pos: String, lexicon: Lexicon = null) = {

    def getLexicalEntries(lemma: String, pos: String, lexicon: Lexicon) = {
      uby.getLexicalEntries(lemma, translatePos(pos), lexicon).asScala
    }

    def senseRelationsForLexicon(lemma: String, pos: String, lexicon: Lexicon) = {
      for (
        entry <- getLexicalEntries(lemma, pos, lexicon);
        sense <- entry.getSenses.asScala;
        rel <- sense.getSenseRelations.asScala; val lexiconName = lexicon.getName; val relName = rel.getRelName;
        val relation = lexiconName + "_" + relName.replace(' ', '_');
        val target = rel.getTarget if target != null;
        targetForm <- target.getLexicalEntry.getLemma.getFormRepresentations.asScala;
        val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
      ) yield (targetLemma, relation)
    }

    def synonymsForLexicon(lemma: String, pos: String, lexicon: Lexicon) = {
      for (
        entry <- getLexicalEntries(lemma, pos, lexicon);
        synset <- entry.getSynsets.asScala;
        val synonymous = synset.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
        targetForm <- synonymous;
        val lexiconName = lexicon.getName;
        val relation = lexiconName + "_synset";
        val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
      ) yield (targetLemma, relation)
    }

    def synsetRelationsForLexicon(lemma: String, pos: String, lexicon: Lexicon) = {
      for (
        entry <- getLexicalEntries(lemma, pos, lexicon);
        synset <- entry.getSynsets.asScala;
        val synonymous = synset.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
        rel <- synset.getSynsetRelations.asScala; val lexiconName = lexicon.getName; val relName = rel.getRelName;
        val relation = lexiconName + "_" + relName.replace(' ', '_');
        val target = rel.getTarget if target != null;
        targetForm <- target.getSenses.asScala.flatMap(_.getLexicalEntry.getLemma.getFormRepresentations.asScala);
        val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
      ) yield (targetLemma, relation)
    }

    List(
      synonymsForLexicon(lemma, pos, lexicon),
      senseRelationsForLexicon(lemma, pos, lexicon),
      synsetRelationsForLexicon(lemma, pos, lexicon)).flatten
  }

  def translatePos(pos: String) = pos match {
    case "n" => EPartOfSpeech.noun
    case "v" => EPartOfSpeech.verb
    case "a" => EPartOfSpeech.adjective
    case "r" => EPartOfSpeech.adverb
    case _   => null
  }

}