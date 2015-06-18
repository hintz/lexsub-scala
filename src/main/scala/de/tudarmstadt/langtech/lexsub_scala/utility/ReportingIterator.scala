package de.tudarmstadt.langtech.lexsub_scala.utility

class ReportingIterator[+A](inner: Iterator[A] ) extends Iterator[A] {
  def hasNext: Boolean = inner.hasNext
  def next(): A = inner.next
}