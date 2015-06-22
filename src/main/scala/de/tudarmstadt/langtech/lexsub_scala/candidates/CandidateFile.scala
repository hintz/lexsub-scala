package de.tudarmstadt.langtech.lexsub_scala.candidates
import de.tudarmstadt.langtech.lexsub_scala.utility
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import de.tudarmstadt.langtech.lexsub_scala.types.{Candidate, CandidateFunction}
import java.util.IllegalFormatException

trait CandidateList extends CandidateFunction {
  val items: Map[String, Seq[Candidate]]
  def get(lemma: String) = items.getOrElse(lemma, Seq.empty)
  
  /** filters this candidate list down to one relation */
  def filterByRelation(relation: String): CandidateList = new FilteredCandidateList(this, relation)
  
  def filteredByAllRelations: Seq[CandidateList] = {
    val relations = for(i <- items.values; c <- i; r <- c.relations) yield r
    relations.toSet.toSeq.map(filterByRelation)
  }
  
  /** outputs the candidate list in the correct format */
  def formatLines: List[String] = items.toList.sortBy(_._1).flatMap { case (target, candidates) => 
    candidates.map { case Candidate(word, pos, replacement, relations) => 
        Seq(word, pos, replacement).mkString("\t") + (if(relations.nonEmpty) relations.mkString("\t", ";", "") else "")
    }
  }
  
  /** writes this candidate list to the given file */
  def save(filename: String) = io.write(filename, formatLines.mkString("\n"))
}

class FilteredCandidateList(val original: CandidateList, relation: String) extends CandidateList {
  lazy val items = {
    val mapped = original.items.mapValues { candidates => candidates.filter { c => c.relations.contains(relation) } }
    val subset = mapped.filter(_._2.nonEmpty)
    subset
  }
  
  override def toString = getClass.getSimpleName + "(" + original + ", relation=" + relation + ")"
}

/*
/** A fixed (in-memory) candidate list. Currently not needed. */
class FixedCandidateList(override val items: Map[String, Seq[Candidate]], name: String = "") extends CandidateList {
  override def toString = getClass.getSimpleName + "(" + name + ")"
}
*/

/** Used to join multiple candidate set. Works lazy (creation is lightweight) */
class JoinedCandidates(union: CandidateList*) extends CandidateList {
 
  // aggregate all sources
  override def get(c: String) =
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
	val c = new CandidateFile("../lexsub-gpl/AIPHES_Data/LexSub/candidates/germeval_duden.tsv", semanticRelationColumn = true)
  val d = new CandidateFile("../lexsub-gpl/AIPHES_Data/LexSub/candidates/germeval_wortschatz.tsv", semanticRelationColumn = true)
  val joined = new JoinedCandidates(c, d)
  val subset = d.filterByRelation("Wortschatz_is_synonym_of")
  subset.formatLines foreach println
  
}