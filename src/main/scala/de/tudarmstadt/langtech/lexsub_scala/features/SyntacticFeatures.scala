package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.utility
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance

case class PosContextWindow(left: Int, right: Int) extends GlobalFeatureExtractor with SimpleNominalFeature[String]  {
  
  val name = "POS_" + left + "_" + right
  val slicer = utility.context[String](left, right) _
  
  def extract(item: LexSubInstance): Seq[Feature] = {
    val sentence = item.sentence
    val posTokens = sentence.tokens.map(_.pos)
    val posWindow = slicer(posTokens, item.headIndex)
    val posWindow2 = posWindow.map { case Some(pos) => pos.substring(0, 1); case None => "x"}
    val value = posWindow2.mkString
    value
  }
}

case class PosContextWindows(leftRange: Range, rightRange: Range, maxSize: Int) 
extends Features((for(l <- leftRange; r <- rightRange; if l + r < maxSize) yield PosContextWindow(l, r)) :_*)