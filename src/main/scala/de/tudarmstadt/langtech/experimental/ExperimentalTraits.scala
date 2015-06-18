package de.tudarmstadt.langtech.experimental

trait ITagset

trait IPosTag {
  def value: String
  def tagset: ITagset
}

trait IToken {
  def word: String
  def lemma: String
  def pos: IPosTag
}