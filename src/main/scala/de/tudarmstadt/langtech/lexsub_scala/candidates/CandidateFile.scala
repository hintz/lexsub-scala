package de.tudarmstadt.langtech.lexsub_scala.candidates
import de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import java.util.IllegalFormatException

case class Candidate(
    /** the lexical item to be replaced */
    val word: String, 
    /** Part-of-Speech */
    val pos: String, 
    /** the replacement word */
    val replacement: String, 
    /** optional set of semantic relation labels extracted from one or more semantic resources */
    val relations: Set[String] = Set.empty) {
  def identity = (word, pos, replacement)
}

trait Candidates {
  val get: Function[String, Seq[Candidate]]
}

trait CandidateList extends Candidates {
  val items: Map[String, Seq[Candidate]]
  val get: Function[String, Seq[Candidate]] = items.getOrElse(_, Seq.empty)
  
  /** outputs the candidate list in the correct format */
  def formatLines: List[String] = items.toList.sortBy(_._1).flatMap { case (target, candidates) => 
    candidates.map { case Candidate(word, pos, replacement, relations) => 
        Seq(word, pos, replacement).mkString("\t") + (if(relations.nonEmpty) relations.mkString("\t", ";", "") else "")
    }
  }
  
  /** writes this candidate list to the given file */
  def save(filename: String) = io.write(filename, formatLines.mkString("\n"))
}

class JoinedCandidates(union: CandidateList*) extends CandidateList {
 
  // aggregate all sources
  override val get: Function[String, Seq[Candidate]] =  (c: String) => 
   union.flatMap(_.get(c)).groupBy(_.identity)
   .mapValues(_.map(_.relations).reduce(_ union _))
   .map { 
     case ((word, pos, replacement), rels)  => Candidate(word, pos, replacement, rels) 
    }
    .toSeq
   
 override def toString = getClass.getSimpleName + "(%s)".format(union.mkString(","))
 
 // only needed for export to file
 override lazy val items = {
   val keySetList = union.map(_.items.keySet).reduce(_ union _).toList // ordered list of keyset
   val results = keySetList.map { key => 
     
   }
   
   keySetList.zip(keySetList.map(get)).toMap
 }
}

/**
 * Reader for candidate files of various formats
 * Simple format: word [tab] pos [tab] replacement    <-- compatible to LexSub
 * Weighted format: word [tab] pos [tab] replacement [tab] [weight as float]    <-- compatible to LexSub
 * Extended format: word [tab] pos [tab] replacement [tab] [semantic_relation [;] ...]
 */
class CandidateFile(val filename: String, val semanticRelationColumn: Boolean = false) extends CandidateList {
  lazy val items: Map[String, Seq[Candidate]] = {
    val parsed = io.lines(filename).map(_.trim.split('\t').toList)
    val candidates = parsed.map { 
      case a :: b :: c :: rel :: rest if semanticRelationColumn => Candidate(a, b, c, rel.split(";").toSet)
      case a :: b :: c :: rest if !semanticRelationColumn => Candidate(a, b, c);
      case _ => throw new Exception }
    candidates.toSeq.distinct.groupBy(_.word)
  }
  override def toString = getClass.getSimpleName + "(" + filename.split("/").last + ")"
}

object TestCandidateFileReader extends App {
	val c = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_wortschatz_all.tsv", semanticRelationColumn = true)
  val d = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_wortschatz_all.tsv", semanticRelationColumn = true)
  
  val joined = new JoinedCandidates(c, d)
  joined.formatLines foreach println
}