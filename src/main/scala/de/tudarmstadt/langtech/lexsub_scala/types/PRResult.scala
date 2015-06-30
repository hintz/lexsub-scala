package de.tudarmstadt.langtech.lexsub_scala.types
import de.tudarmstadt.langtech.scala_utilities.compute

case class PRResult(val tp: Int, val fp: Int, val fn: Int, val tn: Option[Int]) {
  def retrieved = tp + fp
  def relevant = tp + fn
  def +(other: PRResult) = PRResult(tp + other.tp, fp + other.fp, fn + other.fn, for(a <- tn; b <- other.tn) yield a + b)
  def precision: Double = tp / retrieved.toDouble
  def recall: Double = tp / relevant.toDouble
  def fmeasure: Double = compute.fmeasure(precision, recall)
  override def toString = "R=%.2f P=%.2f F1=%.2f".format(recall, precision, fmeasure)
}