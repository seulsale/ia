package alineaciondesecuencias

import scala.io.Source
import scala.util.Random

object Main extends App {
  var human = Source.fromResource("secuencias/humano/env - HIV1H.txt").toList
  var chimpanzee = Source.fromResource("secuencias/chimpance/env - HIV1S.txt").toList

  println(f"Initial similarity ${Population.getSimilarity(human, chimpanzee)}")
  println(f"Similarity after mutation ${Population.getSimilarity(Population.mutation(human, 2), Population.mutation(chimpanzee, 1))}")

  var limit = 50
  var p = 1
  while (p < limit) {

    p += 1
  }
}

object Population {
  def crossPopulations(gen1: List[Char], gen2: List[Char]): (List[Char], List[Char]) = {
    // TODO: Missing implementation
    (gen1, gen2)
  }

  /**
   * Add dashes between the list of chars, the quantity of dashes depends on the percentage given
   * @param gen list of chars
   * @param percentage percentage number
   * @return a list containing random dashes in between
   */
  def mutation(gen: List[Char], percentage: Int): List[Char] = {
    val mutations = Math.round(gen.length/100*percentage.toFloat)
    var newGen: List[Char] = gen.map(c => c)
    for(i <- 1 to mutations) {
      val index = Random.between(0, newGen.length)
      newGen = newGen.take(index) ++ ('-' :: newGen.drop(index))
    }
    newGen
  }

  /**
   * Calculates the minimum length of two lists
   * @param gen1 first gen
   * @param gen2 second gen
   * @return the minimum length of two lists
   */
  def getMinLength(gen1: List[Char], gen2: List[Char]): Int = {
    val len1 = gen1.length
    val len2 = gen2.length
    if (len1 < len2) len1 else len2
  }

  /**
   * Calculates percentage of similarity between two sequences
   * @param gen1 List of characters of the first gen
   * @param gen2 List of characters of the second gen
   * @return the percentage of similarity ranging from 0 to 1
   */
  def getSimilarity(gen1: List[Char], gen2: List[Char]): Double = {
    val maxLimit = getMinLength(gen1, gen2)
    var index: Int = 0
    var same: Int = 0
    while (index < maxLimit) {
      if (gen1(index) == gen2(index)) same += 1
      index += 1
    }
    same.toFloat / maxLimit
  }
}
