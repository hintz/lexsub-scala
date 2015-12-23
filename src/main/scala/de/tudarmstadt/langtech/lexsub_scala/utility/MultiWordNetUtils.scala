package de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.scala_utilities.io

import org.itc.mwn._
import java.util.Date
import java.io._
import java.nio.charset.Charset
import scala.collection.JavaConversions._

class MultiWordNetUtils(language: String) {
  
  // Load Dictionary
  MysqlDictionary.encoding = "UTF-8" // what an API..
  val dictionary = new MysqlDictionary

  def lookup(word: String, pos: String, relationDepth: Int = 2) =  {
    val postag = POS.CATS.collectFirst { case p if p.getKey == pos => p }.get
    //val searchwords = dictionary.searchIndexWords(postag, word, language)
    
    def aggregateLemmas(target: PointerTarget) = target match {
      case synset: Synset =>
        val words = Option(synset.getWords).getOrElse(Array.empty[Word])
        words.map(_.getLemma).toList
      case word: Word => List(word.getLemma)
    }

    def aggregateRelations(synset: Synset): List[(String, String)] = {
      def walk(pointer: Pointer): List[(String, String)] = {
          var result = List.empty[(String, String)]
          val todo = new collection.mutable.Stack[(Pointer, Int)]
          todo.push((pointer, 0))
          while(todo.nonEmpty){
            val (pointer, level) = todo.pop
            val target = pointer.getTarget
            val label = pointer.getType.getLabel + "_" + level
            val words = aggregateLemmas(target)
            val elements = words.zipAll(Seq.empty, null, label)
            result :::= elements
            if(level < relationDepth)
              for(p <- target.getPointers(pointer.getType)) 
                todo.push((p, level + 1))
          }
          result.sortBy(_._2)
      }
      
      synset.getPointers.flatMap(walk).toList
    }
    
    val maybeIndex = Option(dictionary.lookupIndexWord(postag, word, language))
    val senses = maybeIndex.toSeq.flatMap { i =>
      i.getSenses match { case null => Array.empty[Synset]; case a => a}
    }
    val subst = for (
       synset <- senses;
       val related = aggregateRelations(synset);
       val synonymous = aggregateLemmas(synset).zipAll(Seq.empty, null, "synonym");
       w: (String, String) <- synonymous ::: related) yield w
       
    subst
  }
  
}

// Example:

object TestMWN extends App {
  new MultiWordNetUtils("italian").lookup("sostanza", "n") foreach println
}

