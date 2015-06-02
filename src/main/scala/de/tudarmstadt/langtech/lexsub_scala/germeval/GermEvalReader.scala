package de.tudarmstadt.langtech.lexsub_scala.germeval
import scala.xml.XML
import de.tudarmstadt.langtech.lexsub_scala.utility.io
import java.util.IllegalFormatException


case class Item(val sentence: Sentence, val gold: GoldItem)
case class Sentence(val id: String, val sentence: String, val target: String)
case class LexItem(val word: String, val pos: String)
case class GoldItem(val id: String, target: LexItem, val substitutions: List[(String, Int)]) {
  def targetWord = target.word
  def substitutionWords = substitutions.map(_._1)
  def substitutionWordsWithoutMultiwords = substitutionWords.filter(!_.contains(" "))
}

class GermEvalGold(goldfile: String) {

  private val MainSplitter = "::"
  private val AnswerSplitter = ";"

  lazy val items: List[GoldItem] = {
    def parseSolution(s: String) = {
      val tmp = s.trim.split(" ").toSeq
      (tmp.init.mkString(" "), tmp.last.toInt)
    }
    def parse(line: String): GoldItem = {
      val Seq(item, solutions) = line.split(MainSplitter).toSeq
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
    for (lexelt <- xml \\ "lexelt"; instance <- lexelt \\ "instance"; context <- lexelt \\ "context") yield {
      val id = (instance \\ "@id").text
      val target = (context \\ "head").text.trim
      val sentence = context.text.trim
      Sentence(id, sentence, target)
    }
  }
}

class GermEvalReader(folder: String, filename: String) {

  val gold = new GermEvalGold(Seq(folder, "/", filename, ".gold").mkString)
  val data = new GermEvalData(Seq(folder, "/", filename, ".xml").mkString)

  lazy val items = {
    val goldItems = gold.items.map(g => g.id -> g).toMap
    data.sentences.map { sentence =>
      Item(sentence, goldItems(sentence.id))
    }
  }
}

object TestGermEvalReader extends App {
  val reader = new GermEvalReader("AIPHES_Data/GermEval2015", "train-dataset")
  reader.items foreach println
}