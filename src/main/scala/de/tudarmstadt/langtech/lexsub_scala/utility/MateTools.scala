package de.tudarmstadt.langtech.lexsub_scala.utility

import is2.data.SentenceData09
import is2.lemmatizer.Lemmatizer
import is2.parser.Parser
import is2.tag.Tagger


object MateTools {
  

  
  val Models = Map(
      "en" -> Map(
          "tagger" -> "resources/models/mate/tagger-eng-4M-v36.mdl",
          "lemmatizer" -> "resources/models/mate/lemmatizer-eng-4M-v36.mdl",
          "parser" -> "resources/models/mate/parser-eng-12M-v36.mdl"
          ),
      "de" -> Map(
          "lemmatizer" -> "resources/models/mate/lemma-ger-3.6.model",
          "parser" -> "resources/models/mate/parser-ger-3.6.model"
          )
   )
       
  
  def main(args: Array[String]) {
    
    val german = Array("<root>","Peter","hat","eine","Katze",",","die","gerne","Mäuse","fängt",".")
    val english = "Peter has a cat who like catching mice .".split(" ").toArray
    
    val models = Models("en")(_)
    var i = new SentenceData09
    i.init(english)
    
    val tagger = new Tagger(models("tagger"))
    i = tagger(i)

    // create a lemmatizer
    val lemmatizer = new Lemmatizer(models("lemmatizer"));
    i = lemmatizer(i)
    (i.forms, i.ppos, i.plemmas).zipped map { 
      case (form, pos, lemma) => Seq(form, pos, lemma).mkString("\t")
    } foreach println
    
    // initialize the parser
    val parser = new Parser(models("parser"))
    i = parser(i)
    
    // output the result
    System.out.println("FROM\tHEAD\tLABEL")
    (i.forms, i.pheads, i.plabels).zipped  map { 
      case (from, head, label) => Seq(from, head, label).mkString("\t")
    } foreach println
    
  }

}