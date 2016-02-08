package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities._
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance

/** The POS of the target */
case object TargetPosValue extends GlobalFeatureExtractor with NominalFeature[String]  {
  val name = "TypePath(PosPosValue)"
  def extract(item: LexSubInstance): Seq[Feature] = item.head.pos
}

case class PosContextWindow(left: Int, right: Int) extends GlobalFeatureExtractor with NominalFeature[String]  {
  val name = "POS_" + left + "_" + right
  val slicer = collections.context[String](left, right) _
  
  def extract(item: LexSubInstance): Seq[Feature] = {
    val sentence = item.sentence
    val posTokens = sentence.tokens.map(_.pos)
    val posWindow = slicer(posTokens, item.headIndex)
    val posWindow2 = posWindow.map { case Some(pos) => pos.substring(0, 1).toUpperCase; case None => "x"}
    val value = posWindow2.mkString
    value
  }
}

case class PosContextWindows(leftRange: Range, rightRange: Range, maxSize: Int) 
extends Features((for(l <- leftRange; r <- rightRange; if l + r < maxSize) yield PosContextWindow(l, r)) :_*)



case object EditDistance extends LocalFeatureExtractor with NumericFeature {
  val name = getClass.getSimpleName
  val scorer = new edu.stanford.nlp.util.EditDistance
  def extract(item: SubstitutionItem): Seq[Feature] = scorer.score(item.targetLemma, item.substitution)
}
