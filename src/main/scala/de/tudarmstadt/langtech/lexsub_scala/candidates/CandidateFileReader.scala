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

trait CandidateReader extends Candidates {
  val items: Map[String, Seq[Candidate]]
  val get: Function[String, Seq[Candidate]] = items.getOrElse(_, Seq.empty)
}

class JoinedCandidates(union: Candidates*) extends Candidates {
 val get: Function[String, Seq[Candidate]] =  (c: String) => union.flatMap(_.get(c)).distinct
}

case class SimpleCandidate(val word: String, val pos: String, val replacement: String) extends Candidate

class SimpleCandidateFile(val filename: String) extends CandidateReader {
  lazy val items: Map[String, Seq[Candidate]] = {
    val parsed = io.lines(filename).map(_.trim.split('\t').toList)
    val candidates = parsed.map { case a :: b :: c :: rest => SimpleCandidate(a, b, c); case _ => throw new Exception }
    candidates.toSeq.groupBy(_.word)
  }
}

object TestCandidateFileReader extends App {
  val c = new SimpleCandidateFile("AIPHES_Data/LexSub/candidates/germeval_duden.de.txt")
  println(c.get("anspitzen"))
}