package de.tudarmstadt.langtech.lexsub_scala.utility

package lojbanlib.nlp.weka

import java.io.PrintStream
import java.io.ByteArrayOutputStream


case class ARFFFeature(val value: Any, val domain: Seq[String]){
    override def toString = value.toString
 }

/**
 * Writes a given table into ARFF format, as expected by the WEKA ML toolkit
 * None and null values are interpreted as missing ('?')
 * Usage:
 * toARFF("relationname", Seq("somebools", "someints", "somestrings"), 
        List(true, null, false), 
        List(3, 5, null), 
        List("hello", Some("world"), None))
 */
class ARFFWriter(val out: PrintStream) {
  
  /** Automatically computes the domain and writes all colums. CAREFUL! only use for small datasets, otherwise memory may be exceeded */
  def write(relName: String, rows: List[(String, List[Any])]) {
    val (colNames, columns) = rows.unzip
    write(relName, colNames, columns :_*)
  }
  
  /** Automatically computes the domain and writes all colums. CAREFUL! only use for small datasets, otherwise memory may be exceeded */
  def write(relName: String, colNames: Seq[String], columns: List[Any]*) {
    val cols = columns map (c => c map toFeature)
    val domains = cols map toDomain

    writeHeader(relName, colNames, domains)
    for(row <- cols.transpose) 
      out.println(row.mkString(","))
  }
  
  def writeHeader(relName: String, columnNames: Seq[String], columnDomains: Seq[String]){
    out.println("@relation " + relName)
    for ((cName, cDom) <- (columnNames zip columnDomains))
      out.println(List("@attribute", cName, cDom).mkString(" "))
      out.println("@data")
  }
  
  def writeRow(row: Iterable[Any]){
    out.println(row.map(toFeature).mkString(","))
  }
  
  
  def guessDomain(objects: Seq[Any]): String = ARFFWriter.guessDomain(objects)
  def toDomain(items: Seq[ARFFFeature]): String = ARFFWriter.toDomain(items)
  def toFeature(obj: Any): ARFFFeature = ARFFWriter.toFeature(obj)
}

object ARFFWriter {
  
  val NominalThreshold: Int = 1000
  
  
  /** Convenience method returning a string */
  def string(relName: String, colNames: Seq[String], columns: List[Any]*): String = {
    val baos = new ByteArrayOutputStream
    new ARFFWriter(new PrintStream(baos)).write(relName, colNames, columns :_*)
    baos.toString
  }
  
  /** Convenience method returning a string */
  def string(relName: String, rows: List[(String, List[Any])]): String = {
    val (colNames, columns) = rows.unzip
    string(relName, colNames, columns :_*)
  }
  
  
   def toDomain(items: Seq[ARFFFeature]): String = {
    val nonNullItems = items.filter(_.domain.nonEmpty)
    val values = nonNullItems.flatMap(_.domain).toSet
    values.size match {
      case 1 => values.head
      case n if n < NominalThreshold && !values.exists(needsQuote) =>
        nonNullItems.toSet.mkString("{", ", ", "}")
      case _ => "string"
    }
  }

  def toFeature(obj: Any): ARFFFeature = {
    obj match {
      case null => ARFFFeature("?", Seq())
      case None => toFeature(null)
      case Some(o) => toFeature(o)
      case d: Double => ARFFFeature(d.toString, Seq("real"))
      case d: Float => ARFFFeature(d.toString, Seq("real"))
      case i: Integer => ARFFFeature(i.toString, Seq("integer"))
      case o: Object => ARFFFeature(quoteIfNeeded(o.toString), Seq(o.toString))
    }
  }
  
  def guessDomains(domainObjects: Seq[Seq[Any]]): Seq[String] = {
    domainObjects map guessDomain
  }
  
  def guessDomain(objects: Seq[Any]): String = {
    if(objects.isEmpty) "real"
    else toDomain(objects map ARFFWriter.toFeature)
  }
  
  def needsQuote(s: String) = ! (s matches "[a-zA-Z0-9$_]+")
  def quoteIfNeeded(s: String): String = {
    if(needsQuote(s)) "'%s'".format(s.replaceAll("'", "\\\\'")) // not really correct
    else s
  }
}