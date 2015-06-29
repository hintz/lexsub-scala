package de.tudarmstadt.langtech.lexsub_scala.training

import java.io.File
import de.tudarmstadt.langtech.scala_utilities.io
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator

object Training {
  
  val IncludeGoldNotInList = false
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: FeatureAnnotator, trainingDir: File){
    
    println("Starting training on " + data.size + " instances")
    
    // create training data
    val trainingData = createTrainingData(data, candidates)
    println("Using " + candidates + " created " + trainingData.size + " training examples")
    
    // write instances into a file, for later reference
    writeInstances(trainingDir.getPath + "/" + InstanceDataWriter.INSTANCES_OUTPUT_FILENAME, trainingData)
    
    // extract features
    val instances = features(trainingData).flatten
    
    // write classifier data
    val dataWriter = new MalletStringOutcomeDataWriter(trainingDir)
    
    instances foreach dataWriter.write
    dataWriter.finish
    
    //val writer = new InstanceDataWriter[String](trainingDir)
    //dataWriter.getClassifierBuilder.trainClassifier(trainingDir, "MaxEnt")
    //dataWriter.getClassifierBuilder.packageClassifier(trainingDir)
    
    println("Starting train & package..")
    JarClassifierBuilder.trainAndPackage(trainingDir, "MaxEnt")
  }
  
  /** Pairs each LexSubInstance with a number of possible substitutes, based on a candidate list */
  def createTrainingData(data: Iterable[LexSubInstance], candidates: CandidateList): Iterable[Substitutions] = 
  {
    data.map { instance =>
      val headLemma = instance.head.lemma
      val listReplacements = candidates.get(headLemma)
        .collect { case c if c.replacement != headLemma => c.replacement} // exclude headLemma from candidates!
        .toSet
      val replacements =
        if (!IncludeGoldNotInList)
          listReplacements
        else {
          val goldReplacements = instance.gold.map(g => g.gold.substitutionWords)
            .getOrElse(List.empty)
          listReplacements.union(goldReplacements.toSet)
        }
      Substitutions(instance, replacements.toVector)
    }
  }
  
  def writeInstances(filename: String, items: Iterable[Substitutions]){
    val instances = items.flatMap(_.asItems)
    val lines = for((substItem @ SubstitutionItem(instance, substitution)) <- instances) yield {
      val line = Seq(instance.gold.get.gold.id, substitution, substItem.score.get)
      line.mkString("\t")
    }
    io.write(filename, lines.mkString("\n"))
  }
  
  def collectOutcomes(items: Iterable[LexSubInstance], outcomes: Iterable[Seq[(String, Double)]]) = 
    items.zip(outcomes).map(Outcome.tupled)

}