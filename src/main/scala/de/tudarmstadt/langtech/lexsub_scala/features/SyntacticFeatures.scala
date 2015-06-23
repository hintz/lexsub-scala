package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.utility
import org.cleartk.classifier.Feature


// TODO: frequency features

case class PosContextWindow(left: Int, right: Int) extends NominalValueFeatureExtract[Null]("POS_" + left + "_" + right) with PureLocal {
  val slicer = utility.context[String](left, right) _
  
  def extractValue(item: SubstitutionItem, global: Null): String = {
    val sentence = item.lexSubInstance.sentence
    val posTokens = sentence.tokens.map(_.pos)
    val posWindow = slicer(posTokens, item.lexSubInstance.headIndex)
    val posWindow2 = posWindow.map { case Some(pos) => pos.substring(0, 1); case None => "x"}
    val value = posWindow2.mkString
    value
  }
}

case class PosContextWindows(leftRange: Range, rightRange: Range, maxSize: Int) 
extends Features((for(l <- leftRange; r <- rightRange; if l + r < maxSize) yield PosContextWindow(l, r)) :_*)