package de.tudarmstadt.langtech.lexsub_scala

import java.util.Calendar
import java.text.SimpleDateFormat
import java.io.File
import scala.collection.mutable.ListBuffer
import java.io.EOFException
import java.nio.charset.Charset

/**
 * In this package, there are some generally useful helpers, not having to do with Lojban
 * Lojban utilities are in lojbanlib.data.Lojbanutil
 */
package object utility {
  object io {
    
    val FileEncoding = "UTF-8"

    def lines(path: String): Iterator[String] =
      scala.io.Source.fromFile(path)(scala.io.Codec(FileEncoding)).getLines

    def slurp(path: String): String =
      scala.io.Source.fromFile(path, FileEncoding).getLines.mkString("\n")

    def write(file: String, data: String) {
      val stream = new java.io.OutputStreamWriter(new java.io.FileOutputStream(file), FileEncoding)
      stream.write(data)
      stream.close
    }

    def exists(file: String): Boolean = new File(file).exists

    /** More or less a hack. Take raw bytes and try to encode them in default encoding (=UTF-8) */
    def reencode(string: String): String = new String(string.getBytes, FileEncoding)
    
    
    /** Serialize an object in file*/
    def serialize(o: Any, file: String) {
      val s = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))
      s.writeObject(o)
      s.close
    }
    
    /** Load object of type T from file */
    def deserialize[T](file: String): T = {
      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(file))
      val result = s.readObject.asInstanceOf[T]
      s.close
      result
    }

    /** Serialize a sequence of items of type T */
    def serializeSeq[T](file: String, seq: Iterable[T]) {
      val s = new java.io.ObjectOutputStream(new java.io.FileOutputStream(file))
      for (o <- seq) s.writeObject(o)
      s.close
    }

    def deserializeSeq[T](file: String, max: Int = -1): List[T] = {
      val result = new ListBuffer[T]

      val s = new java.io.ObjectInputStream(new java.io.FileInputStream(file))
      try {
        while (max < 0 || result.size < max)
          result += s.readObject.asInstanceOf[T]
      } catch { case x: java.io.EOFException => }
      s.close
      result.toList
    }
    
    /** Lazily serialize an object in a file */
    def lazySerialized[T](filecache: String)(creator: => T): T = {
        if(!new File(filecache).exists){
          val data = creator // fixed point
          serialize(data, filecache)
          data
        }
        else deserialize(filecache)
    }
  }

  object strings {

    def splitAssign(splitter: Char)(string: String): (String, String) = {
      val splitPos = string.lastIndexOf(splitter)
      if (splitPos < 0)
        throw new IllegalArgumentException("Given string %s can not be split at char '%c'".format(string, splitter))
      val (before, after) = string.splitAt(splitPos)
      (before, after.substring(1))
    }

    def md5Hash(text: String): String = {
      java.security.MessageDigest.getInstance("MD5")
        .digest(text.getBytes()).map(0xFF & _)
        .map { "%02x".format(_) }.foldLeft("") { _ + _ }
    }
    
    
    def timestring: String = new SimpleDateFormat("YYYY-MM-dd_kk-mm-ss").format(Calendar.getInstance.getTime)
  }

  def to2Tuple[A](list: List[A]): (A, A) = list match {
    case List(a, b) => (a, b)
  }

  def to3Tuple[A](list: List[A]): (A, A, A) = list match {
    case List(a, b, c) => (a, b, c)
  }

  def to4Tuple[A](list: List[A]): (A, A, A, A) = list match {
    case List(a, b, c, d) => (a, b, c, d)
  }

  /** Wraps an integer into an Option, accepting only positive i */
  def optPositive(i: Int): Option[Int] = if (i >= 0) Some(i) else None

  // TODO: move somewhere else!
  def sublists[A](xs: List[A])(p: A => Boolean): List[List[A]] =
    if (xs.isEmpty) Nil
    else xs span p match { case (a, b) => a :: sublists(b)(x => !p(x)) }

  /**
   * Splits the given list on the given predicate. Per default includes the splitting element itself
   *  at the END of the previous group.
   */
  def splitOn[A](xs: List[A])(p: A => Boolean, includeSplitter: Boolean = true): List[List[A]] = {
    xs.foldRight(List(Nil: List[A])) { (e, acc) =>
      val pred = p(e)
      if (!pred) (e :: acc.head) :: acc.tail
      else (if (includeSplitter) List(e) else Nil) :: acc
    }.filterNot(_.isEmpty)
  }

  /** Extracts a mapping from a list with a leading group-identifying object separating the groups */
  def groupBySplitter[A](xs: List[A])(p: A => Boolean, acc: Map[A, List[A]] = Map.empty[A, List[A]]): Map[A, List[A]] = xs match {
    // Note: tail recursion is absolutely necessary for this to work with longer lists!
    case Nil => acc
    case group :: rest if p(group) =>
      val (taken, dropped) = rest.span(!p(_))
      groupBySplitter(dropped)(p, acc + ((group, taken)))
    case _ :: rest => groupBySplitter(rest)(p, acc)
  }

  def unfoldRight[A, B](seed: B)(f: B => Option[(A, B)]): List[A] = f(seed) match {
    case Some((a, b)) => a :: unfoldRight(b)(f)
    case None => Nil
  }

  def nondeterminism[A, B](f: A => Either[List[A], B])(init: A): List[B] = {
    f(init) match {
      case Right(result) => List(result)
      case Left(options) => options.flatMap(nondeterminism(f))
    }
  }

  def fix[A](init: A, f: A => List[A]): List[List[A]] = {
    var current = f(init)
    var result = List(current)
    while (current.nonEmpty) {
      current = current.flatMap(f)
      result ::= current
    }
    result
  }

  def removeConsecutiveDupes[T]( // todo: needs tail-recursion
    lst: List[T],
    equal: ((T, T) => Boolean) = ((a: T, b: T) => a == b)): List[T] = {
    lst match {
      case head :: next :: tail if equal(head, next) => removeConsecutiveDupes(next :: tail, equal)
      case head :: tail => head :: removeConsecutiveDupes(tail, equal)
      case nil => Nil
    }
  }

  def noNaN(f: Float) = if (f.isNaN) 0f else f
  
  def finiteNumbers(doubles: Iterable[Double]): Iterable[Double] = 
    doubles.filterNot(x => x.isInfinite || x.isInfinite)

  def mean[T](ts: Iterable[T])(implicit num: Numeric[T]) = {
    val n = ts.size
    if(n > 0) num.toDouble(ts.sum) / n
    else 0d
  }
  
  def fmeasure(precision: Double, recall: Double): Double = {
    (2 * recall * precision) / (recall + precision)
  }
   
  /** @return the fraction true items in this collection */
  def fraction(collection: Iterable[Boolean]): Double = {
    collection.count(identity) / collection.size.toDouble
  }

  /** Accumulates weights in a List[(A, Weight)] for all equal elements A */
  def accumulateWeights[A](weighted: Seq[(A, Double)]): List[(A, Double)] = {
    groupLeft(weighted).mapValues(_.sum).toList
  }

  /** Create a map A -> List[B] from the given list */
  def groupLeft[A, B](list: Seq[(A, B)]): Map[A, List[B]] = {
    list.groupBy(_._1).mapValues(_.map(_._2).toList)
  }
  
  /** Flattens a 1-to-N mapping X -> {Y} in a list representation, to a list of tuples (X,Y) */
  def flatten1toN[X, Y](list: List[(X, List[Y])]): List[(X, Y)] = {
    list.flatMap { case (x, ys) => ys.map(y => (x, y)) }
  }

  /** Flips this multimap around:  Map[A, Seq[B]] => Map[B, List[A]] */
  def reverseMap[A, B](multiMap: Map[A, Seq[B]]): Map[B, List[A]] = {
    val swapped = multiMap.toList.flatMap { case (w, keywords) => keywords.map((_, w)) }
    groupLeft(swapped)
  }

  /** Normalizes a set of positive real numbers */
  def normalize(values: Seq[Double]): Seq[Double] = {
    val sum = values.sum
    if(sum > 0) values.map(_ / sum) else values
  }

  def normalizeValues[A](items: Seq[(A, Double)]): Seq[(A, Double)] = {
    val (it, values) = items.unzip
    it.zip(normalize(values))
  }

  /** Create frequency counts.  @param list List containing the types to count */
  def frequency[T](list: Seq[T]): Map[T, Int] = list.groupBy(identity).mapValues(_.size)

  /** Create a frequency distribution map */
  def freqDest[T](list: Seq[T]): Map[T, Double] = frequency(list).mapValues(_.toDouble / list.size)

  /** Compute tf-idf score. @param items list containing lists of items A */
  def tfidf[A](items: Seq[Seq[A]]): Seq[Seq[(A, Double)]] = {
    val termFrequency = frequency(items.map(_.distinct).flatten) // faster, but wrong: frequency(items.flatten)
    val idf = termFrequency.mapValues(f => math.log10(items.size / f.toFloat))
    val tfidf = items.map { it => it.distinct.map(k => (k, frequency(it)(k) * idf(k))) }
    tfidf
  }

  /** Compute tf-idf score and multiply existing weights */
  def wtfidf[A](items: Seq[Seq[(A, Double)]]): Seq[Seq[(A, Double)]] = {
    val termFrequency = frequency(items.map(_.map(_._1).distinct).flatten)
    val idf = termFrequency.mapValues(f => math.log10(items.size / f))
    val tfidf = items.map { it =>
      accumulateWeights(it).map { case (a, f) => (a, f * idf(a)) }
    }
    tfidf
  }

  /** Takes a number of elements from a weighted sequence until max value is reached */
  def takeMaxSum[T](list: List[(T, Double)], max: Double): List[(T, Double)] = {
    val total = list.map(_._2).scan(0d)(_ + _)
    list.zip(total).takeWhile(_._2 < max).map(_._1)
  }
  

  /** Extracts a fixed-size context window from collection */
  def context[A](left: Int, right: Int)(items: IndexedSeq[A], index: Int): IndexedSeq[Option[A]] = {
    val indices = index - left to index + right
    indices.map(items.lift)
  }
}