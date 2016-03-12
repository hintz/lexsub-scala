package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.English
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.German
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.Italian
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

object FilterGoldMWE extends App {
  for(lang <- List(English, German, Italian); 
      sourceFile <- List(lang.testGoldfile, lang.trainGoldfile, lang.cvGoldfile);
      val targetFile = sourceFile + ".nomwe"){
    LexsubUtil.removeMWEfromGold(sourceFile, targetFile)
  }
}