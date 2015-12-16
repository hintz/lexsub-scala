package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem

/** Supplies the id each item ("SentenceId=id") as a feature, useful for debugging */ 
case object SentenceIDFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    Seq(NominalFeature("SentenceId", id))
  }
}

/** Supplies the substitution item ("SubstitutionItem=substitute") as a feature, useful for debugging */ 
case object SubstitutionFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    Seq(NominalFeature("SubstitutionItem", item.substitution))
  }
}

/** Supplies the details of each item ("id=substitute") as a feature, useful for debugging */ 
case object CheatFeature extends LocalFeatureExtractor {
  def extract(item: SubstitutionItem): Seq[Feature] = {
    val id = item.lexSubInstance.gold.map { g => g.gold.id}.getOrElse("no_id")
    val subst = item.substitution
    val value = id + "=" + subst
    Seq(NominalFeature("CheatFeature", value))
  }
}