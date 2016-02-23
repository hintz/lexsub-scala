package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions

trait Feature extends Serializable {
  def name: String
  def value: Any

  /** Converts any feature into a numeric feature */
  def asNumeric: NumericFeature = this match {
    case numeric: NumericFeature => numeric
    case NominalFeature(name, value) => NumericFeature(name + "_" + value, 1d)
    case BinaryFeature(name, value) =>  NumericFeature(name + "_" + value, 1d)
  }
  
  override def toString: String = name + "=" + value
}

object Feature {
  /** Shorthand for empty list of features */
  def nothing = Seq.empty[Feature]
}

case class NumericFeature(val name: String, val value: Double) extends Feature
case class NominalFeature(val name: String, val value: String) extends Feature
case class BinaryFeature(val name: String, val value: Boolean) extends Feature



/** Some utility mixins for defining features */
trait FeatureUtils {

  implicit def noFeatures(implicit item: Substitutions) = Vector.fill(item.candidates.length)(Feature.nothing)

  implicit def asFeature(v: Double)(implicit name: String): Seq[Feature] = Seq(NumericFeature(name, v))
  implicit def asFeature(v: Option[Double])(implicit name: String): Seq[Feature] = v.toList.map(NumericFeature(name, _))
  implicit def asFeature(a: String)(implicit name: String): Seq[Feature] = Seq(NominalFeature(name, a))
}