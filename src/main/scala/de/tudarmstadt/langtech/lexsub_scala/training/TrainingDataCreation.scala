package de.tudarmstadt.langtech.lexsub_scala.training

import de.tudarmstadt.langtech.lexsub_scala.candidates.CandidateList
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.SubstitutionItem
import de.tudarmstadt.langtech.lexsub_scala.types.LexSubInstance

object TrainingDataCreation {
  
  /** Creates training data from LexSubInstances and a candidate list.
   *  Optionally merges candidates in the list with the gold candidates */
  def apply(
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
