package de.tudarmstadt.langtech.lexsub_scala.run

import de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.Settings
import de.tudarmstadt.langtech.lexsub_scala.types.Token

object Scratchpad extends App {
 
  val uby = new de.tudarmstadt.langtech.lexsub_scala.utility.UBYUtil("../AIPHES_Data/UBY/ubymedium070")
  uby.candidatesForLexicon("job", "n", uby.uby.getLexiconByName("WordNet")) foreach { case (w, r) => println(w + "\t" + r)}
  
  val dt = Settings.dts.secondOrder
  dt.similar(Token("job", "n", "job")) foreach { case (w, s) => println(w)}
  
  val wordEmb = Settings.embeddings.levyContexts
  println(wordEmb.apply("door"))


}