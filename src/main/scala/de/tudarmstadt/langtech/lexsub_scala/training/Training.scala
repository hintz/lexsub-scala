package de.tudarmstadt.langtech.lexsub_scala.training

import java.io.File
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types._
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureAnnotator
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder

object Training {
  
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: FeatureAnnotator, trainingDir: File){
    
    println("Starting training on " + data.size + " instances")
    
    // create training data
    val trainingData = createTrainingData(data, candidates, false)
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
  
  
  def createTrainingData(
      data: Iterable[LexSubInstance],
      candidates: CandidateList,
      includeGoldNotInList: Boolean = true): Iterable[SubstitutionItem] = 
  {
    data.flatMap { instance =>
      val headLemma = instance.head.lemma
      val listReplacements = candidates.get(headLemma).map(_.replacement).toSet
      val replacements =
        if (!includeGoldNotInList)
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
}