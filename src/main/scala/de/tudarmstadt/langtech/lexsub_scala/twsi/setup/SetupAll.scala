package de.tudarmstadt.langtech.lexsub_scala.twsi.setup

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateFile
import de.tudarmstadt.langtech.lexsub_scala.candidates.JoinedCandidates
import de.tudarmstadt.langtech.scala_utilities.io

object SetupAll extends App {
  // TODO: call all necessary setup steps
  
  /* Steps needed:
   * 
   * Given an Evaluation set (Semeval-lexsub, GermEval-lexsub, ..)
   * process the resources to setup the system for the given target words. 
   *
   * 1. Crawl online resources for set of target words
   * 1. a. Create per-resource candidate lists
   * 1. b. Merge all lists into "masterlist.tsv" with all candidate expansions
   * 
   * 2. Setup COOC file given candidate set
   * (the cooc file includes coocurence counts for all possible candidates)
   *
   * ..
   *
   */
  
  //CreateTargetList
  
  println("Creating a candidate masterlist..")  
  val wordnet =  new CandidateFile("resources/candidates/twsi/uby_WordNet.tsv", true)
  val ontoWiktionary =  new CandidateFile("resources/candidates/twsi/uby_OntoWiktionaryEN.tsv", true)
  val omegaWiki =  new CandidateFile("resources/candidates/twsi/uby_OmegaWiki_eng.tsv", true)
  
  val masterlist = new JoinedCandidates(wordnet, omegaWiki, ontoWiktionary)
    //.filter(!_.replacement.contains(" ")) // removes Multi Word Expressions from the masterlist
  
  println("Creating vocab file")
  io.write("vocab.txt", masterlist.allItems.mkString("\n"))
  
  println("Saving masterlist")
  masterlist.save("resources/candidates/twsi/twsi_masterlist.tsv")
  
}