package de.tudarmstadt.langtech.lexsub_scala.training.ctk

import java.io.File

import org.cleartk.classifier.Instance
import org.cleartk.classifier.feature.transform.InstanceDataWriter
import org.cleartk.classifier.jar.JarClassifierBuilder
import org.cleartk.classifier.mallet.MalletStringOutcomeDataWriter

import de.tudarmstadt.langtech.lexsub_scala.LexSubExpander
import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.features.Features
import de.tudarmstadt.langtech.lexsub_scala.reader.SemEvalResultOutcomeWriter
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKInstanceBuilder
import de.tudarmstadt.langtech.lexsub_scala.scorer.CTKScorer
import de.tudarmstadt.langtech.lexsub_scala.training.Model
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance
import de.tudarmstadt.langtech.lexsub_scala.types.Outcomes
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.Substitutions
import de.tudarmstadt.langtech.scala_utilities.io

object ClearTKModel extends Model {
  
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String){
    CTKTraining.train(data, candidates, features, trainingFolder)
  }
  
  def getScorer(trainingFolder: String) = CTKScorer(trainingFolder)
}

/** helper object for training a ClearTK classifier */
object CTKTraining {
  
  val Classifier = "MaxEnt" // classifier string as expected by cleartk
  val IncludeGoldNotInList = false // add gold items which are not in candidate list to training. Setting this to true drastically hurts performance!
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  def featurize(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features): Iterable[Instance[String]] = {
    
    // create training data
    val trainingData = createTrainingData(data, candidates)
    println("Using %d instances with candidates from %s created %d training examples".format(data.size, candidates, trainingData.map(_.candidates.size).sum))
    
    // write instances into a file, for later reference
    // writeInstances(trainingDir.getPath + "/" + InstanceDataWriter.INSTANCES_OUTPUT_FILENAME, trainingData)
    
    // extract features
    val instanceMaker = new CTKInstanceBuilder(features)
    val instances = instanceMaker(trainingData).flatten
    instances
  }
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  def train(data: Iterable[LexSubInstance], candidates: CandidateList, features: Features, trainingFolder: String){
    val instances = featurize(data, candidates, features)
    trainAndPackage(instances, trainingFolder)
  }
  
  /** Trains a classifier with the given training data in the directory given in trainingDir */
  @Deprecated
  def trainOnGold(data: Iterable[LexSubInstance], features: Features, trainingFolder: String){
    
    // create training data
    val trainingData = createTrainingDataFromGold(data)
    println("Using %d instances with candidates from gold created %d training examples".format(data.size, trainingData.map(_.candidates.size).sum))
    
    // write instances into a file, for later reference
    val trainingDir = new java.io.File(trainingFolder)
    writeInstances(trainingDir.getPath + "/" + InstanceDataWriter.INSTANCES_OUTPUT_FILENAME, trainingData)
    
    // extract features
    val instanceMaker = new CTKInstanceBuilder(features)
    val instances = instanceMaker(trainingData).flatten
    
    trainAndPackage(instances, trainingDir)
  }
  
  /** Performs crossvalidate, prints results to stdout and writes aggregated results to outputFile */
  def crossvalidate(data: Iterable[LexSubInstance], 
      trainingList: CandidateList, systemList: CandidateList, features: Features, 
      trainingRoot: String, outputFile: String, folds: Int = 10, maxItems: Int = 20){
    
    println("Starting crossvalidation on " + data.size + " instances")
    
    val trainingData = createTrainingData(data, trainingList)
    println("Using %d instances with candidates from %s created %d training examples".format(data.size, trainingList, trainingData.map(_.candidates.size).sum))
    
    println("Extracting features..")
    val instanceMaker = new CTKInstanceBuilder(features)
    val feats = instanceMaker(trainingData)
    
    val dataWithFeatures = trainingData.zip(feats)
    println("Writing " + dataWithFeatures.size + " items into feature cache..")


    println("Grouping data and creating folds..")
    val grouping = (instance: LexSubInstance) => instance.gold.get.sentence.target.lemma
    val grouped = dataWithFeatures.groupBy(x => grouping(x._1.lexSubInstance))
    val folded = crossfold(grouped.keys.toSeq, folds)
    
    val outcomes = for(((heldOutItems, trainingItems), i) <- folded.zipWithIndex) yield {
      val trainingFolder = trainingRoot + "/fold" + i
      val folder = new File(trainingFolder); folder.mkdir
      println("Fold %d (items %s)".format(i + 1, heldOutItems.mkString(", ")))
      val trainingData: Iterable[Instance[String]] = trainingItems.flatMap(grouped.apply).flatMap(_._2)
      trainAndPackage(trainingData, folder)
      
      val testData: Seq[Substitutions] = heldOutItems.flatMap(grouped.apply).map(_._1)
      val testInstaces = testData.map(_.lexSubInstance)
      
      val lexsub = LexSubExpander(systemList, features, CTKScorer(trainingFolder), maxItems = maxItems)
      val ranked = lexsub(testInstaces)
      val results = Outcomes.collect(testInstaces, ranked)
      val oot = Outcomes.evaluate(results, 10)
      val best = Outcomes.evaluate(results, 1)
      println("Fold %d: best=%s oot=%s".format(i + 1, best, oot))
      results
    }
    
    SemEvalResultOutcomeWriter.save(outcomes.flatten, outputFile)
    io.write(outputFile + ".system.txt", LexSubExpander(systemList, features, null, maxItems = maxItems).toString)
    
    val results = outcomes.flatten
    val oot =  Outcomes.evaluate(results, 10)
    val best = Outcomes.evaluate(results, 1)
    println("Overall best=[%s] oot=[%s]".format(best, oot))
  }
  
  
  def crossfold[A](items: Seq[A], folds: Int): Iterable[(Seq[A], Seq[A])] = {
    val heldOutSize = items.size / folds
    for(i <- 0 to folds) yield {
      val start = i * heldOutSize; val end = if(i == folds) items.size else start + heldOutSize
      val heldOut = items.slice(start, end)
      val rest = items.take(start) ++ items.drop(end)
      (heldOut, rest)
    }
  }
  
  def holdOut[A](items: Seq[A], holdOutPercentage: Double): (Seq[A], Seq[A]) = {
    val heldOutSize = (items.size * holdOutPercentage).toInt
    items.splitAt(heldOutSize).swap
  }
  
 /** Calls the training algorithm of the ML backend */
 def trainAndPackage(instances: Iterable[Instance[String]], trainingDir: File){

    println("Training on " + instances.size + " instances.. Writing training data to " + trainingDir)
    val dataWriter = new MalletStringOutcomeDataWriter(trainingDir)
    instances foreach dataWriter.write
    dataWriter.finish
    
    //val writer = new InstanceDataWriter[String](trainingDir)
    //dataWriter.getClassifierBuilder.trainClassifier(trainingDir, "MaxEnt")
    //dataWriter.getClassifierBuilder.packageClassifier(trainingDir)
    
    println("Starting train & package..")
    JarClassifierBuilder.trainAndPackage(trainingDir, Classifier)
  }
 
  /** Calls the training algorithm of the ML backend */
  def trainAndPackage(instances: Iterable[Instance[String]], trainingFolder: String) {
    trainAndPackage(instances, new java.io.File(trainingFolder))
  }
 
 private def createTrainingDataFromGold(data: Iterable[LexSubInstance]): Iterable[Substitutions] = {
   data.map { x => Substitutions(x, x.getGold.gold.substitutionWordsWithoutMultiwords.toVector) }
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
  
}