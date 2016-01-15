package de.tudarmstadt.langtech.lexsub_scala.run.germeval2015.setup

import de.tudarmstadt.langtech.scala_utilities.{ io, strings }
import de.tudarmstadt.ukp.lmf.transform.DBConfig
import de.tudarmstadt.ukp.lmf.api.Uby
import scala.collection.JavaConverters._
import de.tudarmstadt.ukp.lmf.model.core.Lexicon

object CreateUBYCandidates extends App {

  val targets = io.lines("resources/germeval2015/targets-pos.txt").map(strings.splitAssign('\t')).toList
  val ubyDatabase = "../AIPHES_Data/UBY/ubyGermeval060"

  val uby = new Uby(new DBConfig(
    "jdbc:h2:file:" + ubyDatabase,
    "org.h2.Driver", "h2", "sa", "", false))

  System.out.println("Creating candidates from UBY database with the following lexicons: " + uby.getLexiconNames);

  for (lexiconName <- uby.getLexiconNames.asScala; val lexicon = uby.getLexiconByName(lexiconName)) {
    val outputFile = "resources/germeval2015/candidates/germeval_uby_" + lexiconName + ".tsv"
    val lines = for ((lemma, pos) <- targets; (expansion, label) <- candidatesForLexicon(lemma, lexicon))
      yield Seq(lemma, pos, expansion, label).mkString("\t")
    if(lines.length > 1) // UBY might contain some useless dictionaries (wrong language, etc.)
      io.write(outputFile, lines.sorted.distinct.mkString("\n"))
  }
  
  println("Done.")

  def candidatesForLexicon(lemma: String, lexicon: Lexicon = null) = {
    for (
      entry <- uby.getLexicalEntries(lemma, lexicon).asScala; sense <- entry.getSenses.asScala;
      rel <- sense.getSenseRelations.asScala; val lexiconName = lexicon.getName; val relName = rel.getRelName;
      val relation = lexiconName + "_" + relName;
      val target = rel.getTarget if target != null;
      targetForm <- target.getLexicalEntry.getLemma.getFormRepresentations.asScala;
      val targetLemma = targetForm.getWrittenForm if targetLemma != lemma
    ) yield (targetLemma, relation)
  }
}