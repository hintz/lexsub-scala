package de.tudarmstadt.langtech.lexsub_scala.utility

import scala.sys.process.ProcessBuilder
import scala.sys.process.stringSeqToProcess

import de.tudarmstadt.langtech.lexsub_scala.LexSub
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.scala_utilities.io

/** Commandline wrapper for semeval perl score script */
class SemEvalScorer(val scriptFolder: String) {

  def score(instancesFilePrefix: String, goldFile: String): String = {
    import scala.sys.process._

    val scorePL = scriptFolder + "/score.pl"
    val scoreGAPpl = scriptFolder + "/scoreGAP.pl"
    val bestInstances = instancesFilePrefix + ".best"
    val ootInstances = instancesFilePrefix + ".oot"
    val rankInstances = instancesFilePrefix + ".rank"

    val bestCmd: ProcessBuilder = Seq("perl", scorePL, bestInstances, goldFile, "-t", "best")
    val ootCmd: ProcessBuilder = Seq("perl", scorePL, ootInstances, goldFile, "-t", "oot")
    val gapCmd: ProcessBuilder = Seq("perl", scoreGAPpl, rankInstances, goldFile)
    
    val best = bestCmd.lineStream_!
    val oot = ootCmd.lineStream_!
    val gap = gapCmd.lineStream_!

    (Seq("Best:") ++ best ++ Seq("OOT:") ++ oot ++ Seq("GAP:") ++ gap).mkString("\n")
  }
}

object SemEvalScorer {

  /** Run full evaluation (all perl scripts & custom evaluation) */
  def saveAndEvaluate(
    lexsub: LexSub,
    evaluationData: Iterable[LexSubInstance],
    outcomes: Iterable[Seq[(String, Double)]],
    scorerFolder: String,
    goldFile : String,
    folder: String): String = {

    val results = Outcomes.collect(evaluationData, outcomes)
    val instanceFilePrefix =  folder + "/instances.out"
    SemEvalResultOutcomeWriter.save(results, instanceFilePrefix)
    io.write(folder + "/system.txt", lexsub.toString)

    val oot = Outcomes.evaluate(results, 10)
    val best = Outcomes.evaluate(results, 1)
    val myEval = "Evaluation: best=[%s] oot=[%s]".format(best, oot)
    val perlEval = new SemEvalScorer(scorerFolder).score(instanceFilePrefix, goldFile)
    val fullEval = Seq(myEval, perlEval).mkString("\n")
    
    io.write(folder + "/result.txt", fullEval)
    fullEval
  }
  
  def singleLine(evalOutput: String): String = {
    // hacky grep for one line in the output
    val gapLine =  evalOutput.lines.filter(_.contains("GAP score:")).toList.applyOrElse(0, (_: Int) => "ERROR")
    val pBestLine = evalOutput.lines.filter(_.startsWith("precision =")).toList.applyOrElse(0, (_: Int) => "ERROR")
    pBestLine + gapLine
  }
}