package de.tudarmstadt.langtech.lexsub_scala.multiwordnet

import org.itc.mwn._
import java.util.Date
import java.io._
import java.nio.charset.Charset
import scala.collection.JavaConversions._

class MultiWordNetUtils(language: String) {
  
  // Load Dictionary
  val dictionary = new MysqlDictionary

  def lookup(word: String, pos: String, relationDepth: Int = 10) {
    val postag = POS.CATS.collectFirst { case p if p.getKey == pos => p }.get
    //val searchwords = dictionary.searchIndexWords(postag, word, language)
    
    def aggregateLemmas(synset: Synset): List[String] = {
      val words = Option(synset.getWords).getOrElse(Array.empty[Word])
      words.map(_.getLemma).toList
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
            val words = aggregateLemmas(target.asInstanceOf[Synset])
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
    val subst = for (
       indexword <- maybeIndex.toSeq;
       synset <- indexword.getSenses;
       val related = aggregateRelations(synset);
       val synonymous = aggregateLemmas(synset).zipAll(Seq.empty, null, "synonym");
       w: (String, String) <- synonymous ::: related) yield w
       
    println(subst)
  }
  
}

// Example:
/*
object TestMWN extends App {
  new MultiWordNetUtils("italian").lookup("parlare", "v")
}
*/
