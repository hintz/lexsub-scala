package de.tudarmstadt.langtech.lexsub_scala.germeval
import scala.xml.XML
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import de.tudarmstadt.langtech.lexsub_scala.utility.strings
import de.tudarmstadt.langtech.lexsub_scala.types.Token
import java.util.IllegalFormatException

case class GermEvalItem(val sentence: Sentence, val gold: GoldItem)
case class Sentence(val id: String, val sentence: String, val target: Token)
case class LexItem(val word: String, val pos: String)
case class GoldItem(val id: String, target: LexItem, val substitutions: List[(String, Int)]) {
  def targetWord = target.word
  def substitutionWords = substitutions.map(_._1)
  def substitutionWordsWithoutMultiwords = substitutionWords.filter(!_.contains(" "))
}

class GermEvalGold(goldfile: String) {

  lazy val items: List[GoldItem] = {
    def parseSolution(s: String) = {
      val tmp = s.trim.split(" ").toSeq
      (tmp.init.mkString(" "), tmp.last.toInt)
    }
    def parse(line: String): GoldItem = {
      val Seq(item, solutions) = line.split("::").toSeq
      val Seq(lexItem, id) = item.split(" ", 2).toSeq
      val Seq(word, pos) = lexItem.split('.').toSeq
      val substitutions = solutions.split(";").map(parseSolution).toList
      GoldItem(id.trim, LexItem(word, pos), substitutions)
    }
    io.lines(goldfile).map(parse).toList
  }
}

class GermEvalData(datafile: String) {
  lazy val sentences: Seq[Sentence] = {
    val xml = XML.loadFile(datafile)
    for (lexelt <- xml \\ "lexelt"; val lemmaPos = (lexelt \\ "@item").text;
        instance <- lexelt \\ "instance"; val id = (instance \\ "@id").text;
        context <- lexelt \\ "context"; val target = (context \\ "head").text.trim) yield {
      val sentence = context.text.trim
      val (lemma, pos) = strings.splitAssign('.')(lemmaPos)
      Sentence(id, sentence, Token(target, pos, lemma))
    }
  }
}

object GermEvalData {
  
  /** Utility function to load just a sentence with a "head" annotation instead of full XML */
  def parseSentence(s: String): Sentence = {
    val context = XML.loadString("<fake>%s</fake>".format(s)) \\ "fake"
    val target = (context \\ "head").text.trim
    val sentence = context.text.trim
    Sentence("no_id", sentence, Token(target, "?", ""))
  }
}

class GermEvalReader(folder: String, filename: String) {

  val gold = new GermEvalGold(Seq(folder, "/", filename, ".gold").mkString)
  val data = new GermEvalData(Seq(folder, "/", filename, ".xml").mkString)

  lazy val items = {
    val goldItems = gold.items.map(g => g.id -> g).toMap
    data.sentences.map { sentence =>
      GermEvalItem(sentence, goldItems(sentence.id))
    }
  }
}

object TestGermEvalReader extends App {
  println(GermEvalData.parseSentence("Das ist ein <head>Test</head>"))
  
  val reader = new GermEvalReader("../lexsub-gpl/AIPHES_Data/GermEval2015", "train-dataset")
  reader.items foreach println
}