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
import de.tudarmstadt.langtech.lexsub_scala.features.FeatureExtractor
import org.cleartk.classifier.Instance
import org.cleartk.classifier.Feature
import de.tudarmstadt.langtech.lexsub_scala.features.PrecomputedFeatureExtractor
import scala.collection.JavaConverters._
import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.ClassifierScorer
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator
import de.tudarmstadt.langtech.lexsub_scala.FeatureAnnotator

object Training {
  
  val IncludeGoldNotInList = false
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: FeatureAnnotator, trainingFolder: String){
    
    println("Starting training on " + data.size + " instances")
    
    // create training data
    val trainingData = createTrainingData(data, candidates)
    println("Using " + candidates + " created " + trainingData.size + " training examples")
    
    // write instances into a file, for later reference
    val trainingDir = new java.io.File(trainingFolder)
    writeInstances(trainingDir.getPath + "/" + InstanceDataWriter.INSTANCES_OUTPUT_FILENAME, trainingData)
    
    // extract features
    val instances = features(trainingData).flatten
    
    trainAndPackage(instances, trainingDir)
  }
  
  
  /** Performs crossvalidate and prints results to stdout */
  def crossvalidate(data: Iterable[LexSubInstance], candidates: CandidateList, features: FeatureAnnotator, trainingRoot: String, folds: Int = 10){
    
    println("Starting crossvalidation on " + data.size + " instances")
    
    val trainingData = createTrainingData(data, candidates)
    println("Using " + candidates + " created " + trainingData.size + " training examples..")
    
    println("Extracting features..")
    val feats = features(trainingData)
    
    val dataWithFeatures = trainingData.zip(feats)
    val featureCache = dataWithFeatures.toMap.mapValues(_.map(_.getFeatures.asScala))
    val featureExtractor = new FeatureAnnotator(PrecomputedFeatureExtractor(featureCache))

    println("Grouping data..")
    val grouped = dataWithFeatures.groupBy(_._1.lexSubInstance.gold.get.sentence.target)
    val folded = crossfold(grouped.keys.toSeq, folds)
    
    val outcomes = for(((heldOutItems, trainingItems), i) <- folded.zipWithIndex) yield {
      val trainingFolder = trainingRoot + "/fold" + i
      val folder = new File(trainingFolder); folder.mkdir
      println("Fold %d (items %s)".format(i + 1, heldOutItems.mkString(", ")))
      val trainingData: Iterable[Instance[String]] = trainingItems.flatMap(grouped.apply).flatMap(_._2)
      trainAndPackage(trainingData, folder)
      
      val testData: Seq[Substitutions] = heldOutItems.flatMap(grouped.apply).map(_._1)
      val testInstaces = testData.map(_.lexSubInstance)
      
      val lexsub = LexSubExpander(candidates, featureExtractor, ClassifierScorer(trainingFolder))
      val ranked = lexsub(testInstaces)
      val results = collectOutcomes(testInstaces, ranked)
      val oot = results.map(_.bestOutOf(10)).reduce(_ + _)
      val best = results.map(_.bestOutOf(1)).reduce(_ + _)
      println("Fold %d: best=%s oot=%s".format(i + 1, best, oot))
      results
    }
    
    val oot = outcomes.flatten.map(_.bestOutOf(10)).reduce(_ + _)
    val best = outcomes.flatten.map(_.bestOutOf(1)).reduce(_ + _)
    println("Average best=[%s] oot=[%s]".format(best, oot))
  }
  
  
  private def crossfold[A](items: Seq[A], folds: Int): Iterable[(Seq[A], Seq[A])] = {
    val heldOutSize = items.size / folds
    for(i <- 0 until folds) yield {
      val start = i * heldOutSize; val end = start + heldOutSize
      val heldOut = items.slice(start, end)
      val rest = items.take(start) ++ items.drop(end)
      (heldOut, rest)
    }
  }
  
 /** Calls the training algorithm of the ML backend */
 private def trainAndPackage(instances: Iterable[Instance[String]], trainingDir: File){

    println("Training on " + instances.size + " instances.. Writing training data to " + trainingDir)
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
