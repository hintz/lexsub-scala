package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.utility
import org.cleartk.classifier.Feature


// TODO: frequency features

case class PosContextWindow(left: Int, right: Int) extends NominalValueFeatureExtract("POS_" + left + "_" + right) {
  val slicer = utility.context[String](left, right) _
  
  def extractValue(item: SubstitutionItem): String = {
    val sentence = item.lexSubInstance.sentence
    val posTokens = sentence.tokens.map(_.pos)
    val posWindow = slicer(posTokens, item.lexSubInstance.headIndex)
    val posWindow2 = posWindow.map { case Some(pos) => pos.substring(0, 1); case None => "x"}
    val value = posWindow2.mkString
    value
  }
}

case class PosContextWindows(leftRange: Range, rightRange: Range) 
extends FeatureExtractorCollection((for(l <- leftRange; r <- rightRange) yield PosContextWindow(l, r)) :_*)