package de.tudarmstadt.langtech.lexsub_scala.run.crosstrain

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.English
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.German
import de.tudarmstadt.langtech.lexsub_scala.run.crosstrain.Settings.Italian
import de.tudarmstadt.langtech.lexsub_scala.training.Featurizer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.training.ranklib.RankLibModel
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.utility.LexsubUtil
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib.LambdaMart
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLib._
import de.tudarmstadt.langtech.lexsub_scala.utility.SemEvalScorer
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import scala.concurrent.ExecutionContext
import java.util.concurrent.Executors
import de.tudarmstadt.langtech.lexsub_scala.utility.RankLibWrapper
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome
import de.tudarmstadt.langtech.lexsub_scala.features.PairFreqRatios

object FindExample extends App {
  
  val sent = German.allData.find { inst => inst.id == "deckeln_19" }.get
  println(sent)
  
  //German.allData.foreach { d => println(d.getGold.sentence.sentence) }
  /*
  val germanRatio = PairFreqRatios(German.ngrams, 0 to 1, 0 to 1, 2)
  
  val instance = Substitutions(sent, Vector("begrenzen", "rügen", "limitieren"))
  println(germanRatio.extract(instance))
  
  /*
  val engRatio = PairFreqRatios(English.ngrams, 0 to 1, 0 to 1, 2)
  val sent = English.allData.find { inst => inst.id == "1922" }.get
  val instance = Substitutions(sent, Vector("remainder", "balance", "respite"))
  println(engRatio.extract(instance))
  * */
  */
}