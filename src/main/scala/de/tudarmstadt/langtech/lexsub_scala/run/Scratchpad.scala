package de.tudarmstadt.langtech.lexsub_scala.run

import de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.Settings
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.utility.MaltProcessing
import de.tudarmstadt.langtech.lexsub_scala.types.SimpleProcessing
import opennlp.tools.postag.POSTaggerME
import java.io.File
import opennlp.tools.postag.POSModel
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.features.SyntaxEmbeddingFeature
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.lexsub_scala.features.SyntacticEmbeddingCombinator.Add

object Scratchpad extends App {
 
  //val uby = new de.tudarmstadt.langtech.lexsub_scala.utility.UBYUtil("../AIPHES_Data/UBY/ubymedium070")
  //uby.candidatesForLexicon("job", "n", uby.uby.getLexiconByName("WordNet")) foreach { case (w, r) => println(w + "\t" + r)}
  
  
  val malt = MaltProcessing(
  tokenizer = _.split(" "), 
  tagger = new SimpleProcessing.PosTagger {
    lazy val tagger: POSTaggerME = new POSTaggerME(new POSModel(new File("resources/models/opennlp/en-pos-perceptron.bin")))
    def apply(tokens: Iterable[String]) = tagger.tag(tokens.toArray)
  },
  lemmatizer = identity,
  maltModel = "resources/models/malt/engmalt.poly-1.7.mco")
  
  
  val parsed = malt.apply("I want to buy the company")
  val item = LexSubInstance(parsed, 3, None)
  val candidates = Vector("acquire", "purchase", "learn")
  val foo = Substitutions(item, candidates)
  
  
  println(Settings.features.extract(foo))
  
  val levysFeature = SyntaxEmbeddingFeature(
      Settings.embeddings.levyWords, 
      Settings.embeddings.levyContexts,
      Add)
 
  val output = levysFeature.extract(foo)
  println(output)
  
  


}