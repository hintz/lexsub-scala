package de.tudarmstadt.langtech.lexsub_scala.utility

import ciir.umass.edu.eval.Evaluator


// Rank
// java -jar RankLib-2.1.jar -load mymodel.txt -rank MQ2008/Fold1/test.txt -metric2T ERR@10 -score out.txt

object RankLibWrapper {
  
  def train(){
    Evaluator.main(Seq("-load", "mymodel.txt", "-rank", "MQ2008/Fold1/test.txt", "-metric2T", "ERR@10", "-score", "out.txt").toArray)
  }
  

}


object TestRankLibWrapper extends App {
  RankLibWrapper.train()
}