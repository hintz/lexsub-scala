package de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.setup

import de.tudarmstadt.langtech.scala_utilities.{ io, strings }
import de.tudarmstadt.ukp.lmf.transform.DBConfig
import de.tudarmstadt.ukp.lmf.api.Uby
import scala.collection.JavaConverters._
import de.tudarmstadt.ukp.lmf.model.core.Lexicon
import de.tudarmstadt.langtech.lexsub_scala.utility.UBYUtil
import de.tudarmstadt.langtech.lexsub_scala.utility.UBYUtil
import de.tudarmstadt.langtech.lexsub_scala.run.semeval2007.Settings

object CreateUBYCandidates extends App {
	val ubyDatabase = "UBY/ubymedium070"
  val ubyUtil = new UBYUtil(ubyDatabase)
  ubyUtil.createCandidates(Settings.targetsPosFile, Settings.resourcesFolder + "/candidates")
}