package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge


trait SyntacticEmbeddingCombinator {
  def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double
  def pcos(cossim: Double): Double = (cossim + 1) / 2d
}

object SyntacticEmbeddingCombinator {
  
  case object WordSimOnly extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = 
      wordCossim.getOrElse(0d)
  }
  
  case object ContextMeanOnly extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double =
      compute.mean(contextCossims)
  }
  
  case object Add extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = 
      (wordCossim.sum + contextCossims.sum) / (contextCossims.length + 1) 
  }
  
  case object BalAdd extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = {
      val c = contextCossims.length
      (c * wordCossim.sum + contextCossims.sum) / (2 * c)
    }
  }
  
  case object Mult extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = {
      val mult = (wordCossim.toList ++ contextCossims).map(pcos).product
      Math.pow(mult, 1 / (1d + contextCossims.length))
    }
  }
  
   case object BalMult extends SyntacticEmbeddingCombinator {
    def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double = {
      val c = contextCossims.length
      val mult = wordCossim.map(Math.pow(_, c)).getOrElse(1d) * contextCossims.map(pcos).product
      Math.pow(mult, 1 / (2 * c))
    }
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
    
extends SmartFeature[SyntacticEmbedding] with FeatureUtils {
  val name = "SyntaxEmb"

  def global(item: LexSubInstance): SyntacticEmbedding = {
    val lemmaEmbedding = wordEmbeddings(item.head.lemma)
    
    val targetTokenIdx = item.headIndex
    val syntaxElements = item.sentence.edges.collect {
      case DepEdge(label, `targetTokenIdx`, to) =>
        label + "_" + item.sentence.tokens(to).lemma
      case DepEdge(label, from, `targetTokenIdx`) => 
        // FIXME: do we really ignore the direction of edges?
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