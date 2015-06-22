package de.tudarmstadt.langtech.lexsub_scala.training

import java.io.File
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureAnnotator
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder
import de.tudarmstadt.langtech.lexsub_scala.types.Outcome

object Training {
  
  val IncludeGoldNotInList = false
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: FeatureAnnotator, trainingDir: File){
    
    println("Starting training on " + data.size + " instances")
    
    // create training data
    val trainingData = createTrainingData(data, candidates)
    println("Using " + candidates + " created " + trainingData.size + " training examples")
    
    
    // extract features
    val instances = features(trainingData)
    
    // write classifier data
    val dataWriter = new MalletStringOutcomeDataWriter(trainingDir)
    
    val writer = new InstanceDataWriter[String](trainingDir)
    instances foreach dataWriter.write
    dataWriter.finish
    
    //dataWriter.getClassifierBuilder.trainClassifier(trainingDir, "MaxEnt")
    //dataWriter.getClassifierBuilder.packageClassifier(trainingDir)
    
    JarClassifierBuilder.trainAndPackage(trainingDir, "MaxEnt")
  }
  
  /** Pairs each LexSubInstance with a number of possible substitutes, based on a candidate list */
  def createTrainingData(data: Iterable[LexSubInstance], candidates: CandidateList): Iterable[SubstitutionItem] = 
  {
    data.flatMap { instance =>
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
      replacements.map { subst =>
        SubstitutionItem(instance, subst)
      }
    }
  }
  
  def collectOutcomes(items: Iterable[LexSubInstance], outcomes: Iterable[Seq[(String, Double)]]) = 
    items.zip(outcomes).map(Outcome.tupled)

}