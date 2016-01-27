package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge
import org.cleartk.classifier.Feature


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
      val norm = if(c > 0) 2 * c else 1
      (c * wordCossim.sum + contextCossims.sum) / norm
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
      val c = contextCossims.length + 1 // this works only reasonable with +1, but not in paper!
      val left = wordCossim.map(pcos).map(Math.pow(_, c)).getOrElse(1d)
      val right = contextCossims.map(pcos).product
      Math.pow(left * right, 1d / (2 * c))
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
    
extends SmartFeature[SyntacticEmbedding] with NumericFeature {
  val name = "SyntaxEmb"
  
  val INVERSE_MARKER = "I" // suffix added to depedge labels to denote inverse direction
  
  def global(item: LexSubInstance): SyntacticEmbedding = {
    val lemmaEmbedding = wordEmbeddings(item.head.lemma)
    
    val targetTokenIdx = item.headIndex
    val syntaxElements = item.sentence.edges.collect {
      case DepEdge(label, `targetTokenIdx`, to) if to >= 0 =>
        val toToken = item.sentence.tokens(to).word.toLowerCase
        label + "_" + toToken
      case DepEdge(label, from, `targetTokenIdx`) if from >= 0 => 
        val fromToken = item.sentence.tokens(from).word.toLowerCase
        label + INVERSE_MARKER  + "_" + fromToken
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
    
    val result = combinator(wordCossim, contextCossims)
    //println(item.targetLemma + " -> " + item.substitution + ": wordsim=" + wordCossim + " ctxsims=" + contextCossims.mkString(", ") +" ==> " + combinator + " = " + result)
    result
  }
}

/** Utility feature generating multiple SyntaxEmbeddingFeatures from different combinators */
case class SyntaxEmbeddingFeatures(
    wordEmbeddings: WordVectorLookup, 
    contextEmbeddings: WordVectorLookup,
    combinators: SyntacticEmbeddingCombinator*)
  extends Features((for (comb <- combinators) yield SyntaxEmbeddingFeature(wordEmbeddings, contextEmbeddings, comb)): _*)