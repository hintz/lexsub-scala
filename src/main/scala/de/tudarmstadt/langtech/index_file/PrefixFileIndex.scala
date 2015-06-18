package de.tudarmstadt.langtech.index_file


/**
 * A byte offset index for a given search string / prefix
 */
@SerialVersionUID(1000L)
trait PrefixFileIndex extends java.io.Serializable {
  def search(prefix: String): (Long, Long)
}

/*
  case class SplitTree(val c: Char, val begin: Long,
      val smaller: Either[SplitTree, Long], 
      val bigger: Either[SplitTree, Long],
      val next: Option[SplitTree]) extends Index with Serializable
  {
    def end: Long = bigger match {
        case Left(tree) => tree.begin
        case Right(end) => end
    }
    
    def search(prefix: String) = search(prefix.toSeq)
    def search(prefix: Seq[Char]): (Long, Long) = prefix match {
      case Seq() => throw new IllegalStateException
      
      case Seq(`c`) => (begin, end)
      
      case Seq(x, xs@_*) if x < c => smaller match {
        case Left(tree) => tree.search(prefix)
        case Right(leftmost) => (leftmost, begin)
      }
      
      case Seq(x, xs@_*) if x > c => bigger match {
        case Left(tree) => tree.search(prefix)
        case Right(rightmost) => (begin, rightmost)
      }
      
      // found the area, recurse to next prefix char
      case `c` :: xs => next match {
        case Some(tree) => tree.search(xs)
        case None => (begin, end)
      }
    }
  }
  */

@SerialVersionUID(1000L)
case class FixedSizePrefixIndex(index: Map[String, Long], end: Long) extends PrefixFileIndex with Serializable {
  val byLength = index.keys.groupBy(_.length).mapValues(_.toVector.sorted).map(identity)
  val maxLength = byLength.keySet.max
  def search(prefix: String): (Long, Long) = {
    val actualPrefix = prefix.take(maxLength) // cap prefix
    val prefixLength = actualPrefix.length // only consider given length
    val subindexKeys = byLength(prefixLength) // extract keys for given length
    val firstLarger = subindexKeys.indexWhere(_ > actualPrefix) // find first key that's bigger
    val result: (Long, Long) = firstLarger match {
      case i if i >= 0 => (index(subindexKeys(i - 1)), index(subindexKeys(i))) // default case: between previous and entry
      case 0           => (0, index(subindexKeys.head)) // must be between beginning and first entry
      case -1          => (index(subindexKeys.last), end) // must be between last entry and end
    }
    result
  }
}