package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge
import org.cleartk.classifier.Feature


trait SyntacticEmbeddingCombinator {
  def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double
}

object SyntacticEmbeddingCombinator {
  case object Add extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = 
      (wordCossim.sum + contextCossims.sum) / (contextCossims.length + 1) 
  }
}


/** For each instance, we retrieve the embedding of the target word and all its syntax elements */
case class SyntacticEmbedding(
    targetEmbedding: Option[breeze.linalg.Vector[Double]], 
    contextSyntaxEmbeddings: List[breeze.linalg.Vector[Double]])

case class SyntaxEmbeddingFeature(
    wordEmbeddings: WordVectorLookup, 
    contextEmbeddings: WordVectorLookup,
    combinator: SyntacticEmbeddingCombinator) 
    
extends SmartFeature[SyntacticEmbedding] with NumericFeature {
  val name = "SyntaxEmb"

  def global(item: LexSubInstance): SyntacticEmbedding = {
    val lemmaEmbedding = wordEmbeddings(item.head.lemma)
    
    val targetTokenIdx = item.headIndex
    val syntaxElements = item.sentence.edges.collect {
      case DepEdge(label, `targetTokenIdx`, to) =>
        label + "_" + item.sentence.tokens(to).lemma
      case DepEdge(label, from, `targetTokenIdx`) =>
        label + "_" + item.sentence.tokens(from).lemma
    }
    
    val syntaxEmbeddings = syntaxElements.flatMap { nsubj_foo =>
      contextEmbeddings(nsubj_foo).toSeq
    }
    
    SyntacticEmbedding(lemmaEmbedding, syntaxEmbeddings)
  }
  
  def extract(item: SubstitutionItem, global: SyntacticEmbedding): Seq[Feature] = {

    val substituteEmbedding = wordEmbeddings(item.substitution).getOrElse {
      return Seq.empty // if the substitute is not present in embedding space, this feature won't work!
    }
    
    // cos-sim between word and target in embedding space
    val wordCossim = global.targetEmbedding.map { targetEmb => 
      LinAlgFunctions.cossim(substituteEmbedding, targetEmb)
     }
    
    // cos-sim between word and target's syntax embedding
    val contextCossims = global.contextSyntaxEmbeddings.map { syntaxEmb =>
      LinAlgFunctions.cossim(substituteEmbedding, syntaxEmb)
    }
    
    combinator(wordCossim, contextCossims)
  }
}