package de.tudarmstadt.langtech.lexsub_scala.features

import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.scala_utilities._
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.DepEdge
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions


trait SyntacticEmbeddingCombinator {
  def apply(wordCossim: Option[Double], contextCossims: List[Double]): Double
  def pcos(cossim: Double): Double = (cossim + 1) / 2d
  def name: String = getClass.getSimpleName.takeWhile(_.isLetterOrDigit)
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
    
    case object NPIC extends SyntacticEmbeddingCombinator {
      def apply(a: Option[Double], b: List[Double]): Double = throw new RuntimeException("unused")
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
    
extends SmartFeature[SyntacticEmbedding] {
  
  // suffix added to depedge labels to denote inverse direction
  val INVERSE_MARKER = "I" 
    
  // support for lowercased / non-lowercased embedding files, based on flag in WordVectorLookup
  val targetLemma: Token => String = if(wordEmbeddings.isLowercased) _.lemma.toLowerCase else _.lemma
  val lexicalPart: Token => String = if(contextEmbeddings.isLowercased) _.word.toLowerCase else _.word
  
  def global(item: LexSubInstance): SyntacticEmbedding = {
    val lemma = targetLemma(item.head)
    val lemmaEmbedding = wordEmbeddings(lemma)
    
    val targetTokenIdx = item.headIndex
    val syntaxElements = item.sentence.edges.collect {
      case DepEdge(label, `targetTokenIdx`, to) if to >= 0 =>
        val toToken = lexicalPart(item.sentence.tokens(to))
        label + "_" + toToken
      case DepEdge(label, from, `targetTokenIdx`) if from >= 0 => 
        val fromToken = lexicalPart(item.sentence.tokens(from))
        label + INVERSE_MARKER  + "_" + fromToken
    }
    
    val syntaxEmbeddings = syntaxElements.flatMap { nsubj_foo =>
      contextEmbeddings(nsubj_foo).toSeq
    }
    
    SyntacticEmbedding(lemmaEmbedding, syntaxEmbeddings)
  }
  
  /*
  def extract(item: Substitutions): Vector[Seq[Feature]] = {
    val g = global(item.lexSubInstance)
    if(combinator == SyntacticEmbeddingCombinator.NPIC){
      if(g.targetEmbedding.isEmpty || g.contextSyntaxEmbeddings.isEmpty) 
        return Vector.fill(item.candidates.length)(Feature.nothing)
        item.asItems.map { case substItem => 
          
        }
        
       
       val st = substituteEmbedding.dot(global.targetEmbedding.get)
       val sctxs = global.contextSyntaxEmbeddings.map { ctxEmb => substituteEmbedding.dot(ctxEmb) }.sum
       val result = math.exp(st) * math.exp(sctxs)
       return Seq(NumericFeature("SyntaxEmb" + combinator.name, result))
    }
    else {
      item.asItems.map(extract(_, g))
    }
  }*/
  
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
    Seq(NumericFeature("SyntaxEmb" + combinator.name, result))
  }
}

/** Utility feature generating multiple SyntaxEmbeddingFeatures from different combinators */
case class SyntaxEmbeddingFeatures(
    wordEmbeddings: WordVectorLookup, 
    contextEmbeddings: WordVectorLookup,
    combinators: SyntacticEmbeddingCombinator*)
  extends Features((for (comb <- combinators) yield SyntaxEmbeddingFeature(wordEmbeddings, contextEmbeddings, comb)): _*)