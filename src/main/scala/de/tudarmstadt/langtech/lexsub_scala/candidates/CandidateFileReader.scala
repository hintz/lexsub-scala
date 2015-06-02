package de.tudarmstadt.langtech.lexsub_scala.candidates
import de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import java.util.IllegalFormatException


trait Candidate {
  def word: String
  def pos: String
  def replacement: String
}

trait Candidates {
  val get: Function[String, Seq[Candidate]]
}

trait CandidateList extends Candidates {
  val items: Map[String, Seq[Candidate]]
  val get: Function[String, Seq[Candidate]] = items.getOrElse(_, Seq.empty)
}

class JoinedCandidates(union: CandidateList*) extends Candidates {
 val get: Function[String, Seq[Candidate]] =  (c: String) => union.flatMap(_.get(c)).distinct.distinct
 override def toString = getClass.getSimpleName + "(%s)".format(union.mkString(","))
}

case class SimpleCandidate(val word: String, val pos: String, val replacement: String) extends Candidate
case class SemRelationCandidate(val word: String, val pos: String, val replacement: String, val relations: List[String]) extends Candidate

/**
 * Reader for candidate files of various formats
 * Simple format: word [tab] pos [tab] replacement
 * Simple format: word [tab] pos [tab] replacement [tab] [semantic_relation [;] ...]
 */
class CandidateFile(val filename: String, val relationFormat: Boolean = false) extends CandidateList {
  lazy val items: Map[String, Seq[Candidate]] = {
    val parsed = io.lines(filename).map(_.trim.split('\t').toList)
    val candidates = parsed.map { 
      case a :: b :: c :: rel :: rest if relationFormat => SemRelationCandidate(a, b, c, rel.split(";").toList)
      case a :: b :: c :: rest if !relationFormat => SimpleCandidate(a, b, c);
      case _ => throw new Exception }
    candidates.toSeq.distinct.groupBy(_.word)
  }
  
  override def toString = getClass.getSimpleName + "(" + filename.split("/").last + ")"
}

object TestCandidateFileReader extends App {
  val c = new CandidateFile("AIPHES_Data/LexSub/candidates/germeval_duden.de.txt")
  println(c.get("anspitzen"))
}