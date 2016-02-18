package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import org.cleartk.classifier.Feature

/** Supplies the id each item ("SentenceId=id") as a feature, useful for debugging */ 
case object SentenceIDFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    Seq(new Feature("SentenceId", id))
  }
}

/** Supplies the substitution item ("SubstitutionItem=substitute") as a feature, useful for debugging */ 
case object SubstitutionFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    Seq(new Feature("SubstitutionItem", item.substitution))
  }
}

/** Supplies the details of each item ("id=substitute") as a feature, useful for debugging */ 
case object CheatFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    val subst = item.substitution
    val value = id + "=" + subst
    Seq(new Feature("CheatFeature", value))
  }
}

/** Supplies a constant feature */ 
case class ConstantFeature(name: String, value: String) extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    Seq(new Feature(name, value))
  }
}