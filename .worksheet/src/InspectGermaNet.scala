
import de.tuebingen.uni.sfs.germanet.api.GermaNet
import scala.collection.JavaConverters._
import de.tuebingen.uni.sfs.germanet.api.ConRel

object InspectGermaNet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(251); 
  val gn = new GermaNet("/Volumes/AIPHES_HDD/AIPHES_Data/GermaNet/GN_V80/GN_V80_XML");System.out.println("""gn  : de.tuebingen.uni.sfs.germanet.api.GermaNet = """ + $show(gn ));$skip(42); 
  val ss = gn.getSynsets("Erleichterung");System.out.println("""ss  : java.util.List[de.tuebingen.uni.sfs.germanet.api.Synset] = """ + $show(ss ));$skip(31); 
  val sample = ss.asScala.head;System.out.println("""sample  : de.tuebingen.uni.sfs.germanet.api.Synset = """ + $show(sample ));$skip(50); 
  
  
  val lexunits = sample.getLexUnits.asScala;System.out.println("""lexunits  : scala.collection.mutable.Buffer[de.tuebingen.uni.sfs.germanet.api.LexUnit] = """ + $show(lexunits ));$skip(35); 
  
  val lexsample = lexunits.head;System.out.println("""lexsample  : de.tuebingen.uni.sfs.germanet.api.LexUnit = """ + $show(lexsample ));$skip(25); val res$0 = 
  
  lexsample.getFrames;System.out.println("""res0: java.util.List[de.tuebingen.uni.sfs.germanet.api.Frame] = """ + $show(res$0));$skip(28); val res$1 = 
  
  lexsample.getWordClass;System.out.println("""res1: de.tuebingen.uni.sfs.germanet.api.WordClass = """ + $show(res$1));$skip(146); 
  
  //sample.getRelatedSynsets(ConRel.has_hyponym).asScala foreach println
  sample.getLexUnits.asScala.head.getSynonyms.asScala foreach println}
}
