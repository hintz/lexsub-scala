package de.tudarmstadt.langtech.lexsub_scala.reader
import scala.xml.XML
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.scala_utilities.strings
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import java.util.IllegalFormatException
import scala.util.{Try, Success, Failure}

case class SemEvalItem(val sentence: Sentence, val gold: GoldItem)
case class Sentence(val id: String, val sentence: String, val target: Token)
case class LexItem(val word: String, val pos: String)
case class GoldItem(val id: String, target: LexItem, val substitutions: List[(String, Int)]) {
  def targetWord = target.word
  def substitutionWords = substitutions.map(_._1)
  def substitutionWordsWithoutMultiwords = substitutionWords.filter(!_.contains(" "))
}

class SemEvalGold(val file: String) {

  lazy val items: List[GoldItem] = {
    
    def parseSolution(s: String) = Try {
      val (word, count) = strings.splitAssign(' ')(s.trim)
      (word, count.toInt)
    }
    
    def parse(line: String): Option[GoldItem] = {
      def warn(s: Any) = System.err.println("WARNING: Could not parse line '%s': %s".format(line, s))
      try {
        val Seq(item, solutions) = line.split("::").toSeq
        if(solutions.trim.isEmpty){
          System.err.println("WARNING: No gold items for " + item)
          return None
        }
        val Seq(lexItem, id) = item.split(" ", 2).toSeq
        val Seq(word, pos) = lexItem.split("\\.", 2).toSeq
        val parsedSolutions = solutions.split("(?<=\\d+);").map(parseSolution)
        for(Failure(e) <- parsedSolutions.filter(_.isFailure)) warn(e)
        val substitutions = parsedSolutions.flatMap(_.toOption).toList
        Some(GoldItem(id.trim, LexItem(word, pos), substitutions))
      } catch {
        case e: Exception => warn(e); None
      }
    }
    
    try { io.lines(file).filter(_.length > 1).flatMap(parse).toList }
    catch { case e: java.io.FileNotFoundException =>
      System.err.println("WARNING: No gold file found: " + file)
      List.empty
     }
  }
}

class SemEvalData(val file: String) {
  lazy val sentences: Seq[Sentence] = {
    val xml = XML.loadFile(file)
    for (lexelt <- xml \\ "lexelt"; val lemmaPos = (lexelt \\ "@item").text;
        instance <- lexelt \\ "instance"; val id = (instance \\ "@id").text;
        context <- instance \\ "context"; val head = (context \\ "head").text.trim) yield {
      val sentence = context.text.trim
      val target = context.child match {
        case Seq(before, _, after) => // target may include stuff outside the XML <head>
           val leftAppend = before.text.reverse.takeWhile(_.isLetter).reverse
           val rightAppend = after.text.takeWhile(_.isLetter)
           leftAppend + head + rightAppend
        case _ => head
      }
      val (lemma, pos) = strings.splitAssign('.')(lemmaPos)
      Sentence(id, sentence, Token(target, pos, lemma))
    }
  }
}

object SemEvalData {
  
  /** Utility function to load just a sentence with a "head" annotation instead of full XML */
  def parseSentence(s: String): Sentence = {
    val context = XML.loadString("<fake>%s</fake>".format(s)) \\ "fake"
    val target = (context \\ "head").text.trim
    val sentence = context.text.trim
    Sentence("no_id", sentence, Token(target, "?", ""))
  }
}

class SemEvalReader(folder: String, datafile: String, goldfile: String) {
  
  /** Constructor for default naming convention
   *  (not all lexsub datasets follow this) */
  def this(folder: String, filename: String) = this(folder, filename + ".xml", filename + ".gold")

  val gold = new SemEvalGold(Seq(folder, "/", goldfile).mkString)
  val data = new SemEvalData(Seq(folder, "/", datafile).mkString)

  lazy val items = {
    val goldItems = gold.items.map(g => g.id -> g).toMap
    def getGold(sentence: Sentence) = 
      goldItems.getOrElse(sentence.id, 
          GoldItem(sentence.id, LexItem(sentence.target.lemma, sentence.target.pos), List.empty))
    data.sentences.map { sentence => SemEvalItem(sentence, getGold(sentence)) }
  }
}

object TestSemEvalReader extends App {
  println(SemEvalData.parseSentence("Das ist ein <head>Test</head>"))
}