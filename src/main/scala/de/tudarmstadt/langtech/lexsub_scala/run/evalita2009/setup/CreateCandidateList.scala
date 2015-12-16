package de.tudarmstadt.langtech.lexsub_scala.run.evalita2009.setup

import de.tudarmstadt.langtech.lexsub_scala.run.evalita2009.Settings
import de.tudarmstadt.langtech.lexsub_scala.utility.MultiWordNetUtils
import de.tudarmstadt.langtech.lexsub_scala.reader.LexItem
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalData
import de.tudarmstadt.langtech.lexsub_scala.types.Token

object CreateCandidateList extends App {
  
  val RelationDepth = 1
  val mwn = new MultiWordNetUtils("italian")
  val data = Settings.evalitaTest ++ Settings.evalitaTrial
  val evalitaData = data.map(_.getGold)
  val targets = evalitaData.map(_.sentence.target).distinct
  //val gold = evalitaData.map(_.gold.target).distinct

  val expansions = for(Token(word, pos, lemma) <- targets;
                       (expanded, relation) <- mwn.lookup(lemma, pos, RelationDepth)
                        if expanded != lemma) 
                          yield Seq(lemma, pos, expanded, relation).mkString("\t")
                       
                       
  io.write(Settings.resourcesFolder + "/candidates/mwn.tsv", expansions.mkString("\n"))
  io.write(Settings.resourcesFolder + "/targets.txt", targets.map(_.lemma).distinct.mkString("\n"))
  io.write(Settings.resourcesFolder + "/targets-pos.txt", targets.map(t => t.lemma + "\t" + t.pos).distinct.mkString("\n"))
}