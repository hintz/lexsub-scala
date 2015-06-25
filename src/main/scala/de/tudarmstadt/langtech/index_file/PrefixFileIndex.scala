package de.tudarmstadt.langtech.index_file


/**
 * A byte offset index for a given search string / prefix
 */
@SerialVersionUID(1000L)
trait PrefixFileIndex extends java.io.Serializable {
  def search(prefix: String): (Long, Long)
}


@SerialVersionUID(1000L)
case class FixedSizePrefixIndex(index: Map[String, Long], end: Long) extends PrefixFileIndex with Serializable {
  val byLength = index.keys.groupBy(_.length).mapValues(_.toVector.sorted).map(identity)
  val maxLength = byLength.keySet.max
  def search(prefix: String): (Long, Long) = {
    val actualPrefix = prefix.take(maxLength) // cap prefix
    val prefixLength = actualPrefix.length // only consider given length
    val subindexKeys = byLength(prefixLength) // extract keys for given length
    val firstLarger = binsearch(subindexKeys, actualPrefix) // find first key that's bigger
    //val firstLargerCheck = subindexKeys.indexWhere(_ > actualPrefix) 
    //assert(firstLarger == firstLargerCheck)
    val result: (Long, Long) = firstLarger match {
      case i if i > 0 => (index(subindexKeys(i - 1)), index(subindexKeys(i))) // default case: between previous and entry
      case 0           => (0, index(subindexKeys.head)) // must be between beginning and first entry
      case -1          => (index(subindexKeys.last), end) // must be between last entry and end
    }
    result
  }

  private def binsearch[A <% Ordered[A]](elems: Vector[A], target: A): Int = {
    var low = 0; var high = elems.size
    while (low != high) {
      val mid = (low + high) / 2
      if (elems(mid) <= target) low = mid + 1
      else high = mid
    }
    /* Now, low and high both point to the element in question. */
    high
  }
}