package de.tudarmstadt.langtech.lexsub_scala.setup

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
   * 3. 
   * 
   * 4.
   * 
   *
   */
  
  //CreateTargetList
  
  
  println("Creating a candidate masterlist..")  
  val germanet =  new CandidateFile("resources/candidates/germeval_germanet90.tsv", true)
  val duden = new CandidateFile("resources/candidates/germeval_duden.tsv", true)
  val woxikon = new CandidateFile("resources/candidates/germeval_woxikon.tsv", true)
  val wortschatzSyn = new CandidateFile("resources/candidates/germeval_wortschatz.tsv", true)
  val masterlist = new JoinedCandidates(germanet, duden /*, woxikon, wortschatzSyn */)
  val masterlistNoMWE = masterlist.filter(!_.replacement.contains(" ")) // removes Multi Word Expressions from the masterlist
  
  println("Creating vocab file")
  io.write("vocab.txt", masterlistNoMWE.allItems.mkString("\n"))
  
  println("Saving masterlist")
  masterlistNoMWE.save("resources/candidates/germeval_masterlist.tsv")
  
}