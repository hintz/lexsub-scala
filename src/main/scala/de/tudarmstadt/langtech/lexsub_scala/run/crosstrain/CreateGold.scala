package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.{ English, German, Italian}
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.reader.LexItem
import de.tudarmstadt.langtech.lexsub_scala.types.Candidate
import de.tudarmstadt.langtech.lexsub_scala.candidates.FixedCandidateList
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil

object CreateGold extends App {
  val pooling = List(
      (English.allData, Settings.path("Candidates", "English", "SemEval2007", "gold")), 
      (German.allData, Settings.path("Candidates", "German", "GermEval2015", "gold")), 
      (Italian.allData, Settings.path("Candidates", "Italian", "Evalita2009", "gold")))
  for ((data, targetFile) <- pooling){
    val candidates = LexsubUtil.poolGoldCandidatesFromInstances(data)
    candidates.save(targetFile)
  }
}