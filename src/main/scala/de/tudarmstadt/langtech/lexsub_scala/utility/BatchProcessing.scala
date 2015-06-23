package de.tudarmstadt.langtech.lexsub_scala.utility

import de.tudarmstadt.langtech.lexsub_scala.utility.ReportingIterable._

/** Utility trait to run apply operation on sequential input in parallel */
trait Parallelizable[In, Out] {
  def apply(input: In): Out
  def parallelApply(input: Seq[In]): Seq[Out] = input.par.map(apply).seq
}

/** Utility trait to run processing step on a collection of input */
trait BatchProcessing[In, Out] extends Parallelizable[In, Out] { 
  def apply(input: In): Out
  
  def report(i: Int, n: Int, passed: Double, remaining: Double){
    println("%d / %d items (%.2f%%) %.0fs passed, %.1fs remaining".format(i, n, i * 100f / n, passed, remaining))
  }
  
  def apply(input: Iterable[In]): Iterable[Out] = input.reporting(report, 5000).map(apply)
}