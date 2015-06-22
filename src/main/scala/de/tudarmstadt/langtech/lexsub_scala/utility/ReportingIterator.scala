package de.tudarmstadt.langtech.lexsub_scala.utility


class ReportingIterator[+A](inner: Iterable[A], reporter: ReportingIterable.Reporter, reportInterval: Long) extends Iterator[A] {
  
  val iter = inner.iterator
  val n = inner.size
  
  var i = 0
  var started = -1l
  var lastReportTimestamp = -1l
  
  def hasNext: Boolean = iter.hasNext
  def next(): A = {
    val now = System.currentTimeMillis
    if(started < 0) started = now
    
    if(now - lastReportTimestamp > reportInterval){
      val passed = (now - started) / 1000d
      val remaining = (n - i) * (passed / (i + 1))
      reporter(i, n, passed, remaining)
      lastReportTimestamp = now
    }
    i += 1
    iter.next
  }
}

class ReportingIterable[A](inner: Iterable[A], reporter: ReportingIterable.Reporter, reportInterval: Long) extends Iterable[A] {
  def iterator: Iterator[A] = new ReportingIterator(inner, reporter, reportInterval)
}

class ReportingWrapper[A](inner: Iterable[A]) {
  def reporting(reporter: ReportingIterable.Reporter, reportInterval: Long): ReportingIterable[A] = 
    new ReportingIterable(inner, reporter, reportInterval)
}


object ReportingIterable {
  type Reporter = (Int, Int, Double, Double) => Unit
  implicit def toReportingWrapper[A](inner: Iterable[A]) = new ReportingWrapper(inner)
}