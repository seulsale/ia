package alineaciondesecuencias

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

object Main extends App {
  var human = Source.fromResource("secuencias/humano/env - HIV1H.txt").toList
  var chimpanzee = Source.fromResource("secuencias/chimpance/env - HIV1S.txt").toList

  var bestPopulation = (human, chimpanzee)
  var bestSimilarity = Population.getSimilarity(human, chimpanzee)

  println(f"Initial Population: $bestPopulation")
  println(f"Initial similarity: $bestSimilarity")

  var limit = 50
  var p = 1
  while (p < limit) {
    println("Creating new population...")
    val (newInd1, newInd2) = Population.crossPopulation(human, chimpanzee)

    println("Getting the best...")
    if (Population.getSimilarity(newInd1, newInd2) > bestSimilarity) {
      println("The new population is better")
      bestPopulation = (newInd1, newInd2)
    } else {
      println("The current population is better")
    }

    p += 1
  }
  println(f"Best population of $limit iterations is: $bestPopulation \nwith ${bestSimilarity*100}%% of similarity.")
}

object Population {
//  def createPopulation(gen1: List[Char], gen2: List[Char], quantity: Int): List[List[Char]] = {
//    val population = ListBuffer[List[Char]]()
//    var currentInd1 = gen1
//    var currentInd2 = gen2
//    for (i <- 1 to quantity) {
//      val (ind1, ind2) = crossPopulation(gen1, gen2)
//      currentInd1 = ind1
//      currentInd2 = ind2
//      population.addOne(currentInd1)
//      population.addOne(currentInd2)
//    }
//    population.toList
//  }

  def crossPopulation(ind1: List[Char], ind2: List[Char]): (List[Char], List[Char]) = {
    val crossPoint = Random.between(0, getMinLength(ind1, ind2))
    val firstIndividual = getFirst(ind1, crossPoint) ++ getLast(ind2, crossPoint)
    val secondIndividual = getLast(ind1, crossPoint) ++ getFirst(ind2, crossPoint)
    val mutateFirstInd = mutation(firstIndividual, 5)
    val mutateSecondInd = mutation(secondIndividual, 5)
    val doesMutate = Random.between(0, 4)
    var result = (firstIndividual, secondIndividual)
    if (doesMutate == 1) result = (mutateFirstInd, secondIndividual)
    if (doesMutate == 2) result = (firstIndividual, mutateSecondInd)
    if (doesMutate == 3) result = (mutateFirstInd, mutateSecondInd)
    result
  }

  /**
   * Return the first crossPoint characters skipping dashes
   * @param gen list of characters
   * @param crossPoint limit of characters for the new list
   * @return a list of characters up to crossPoint skipping dashes
   */
  def getFirst(gen: List[Char], crossPoint: Int): List[Char] = {
    var i = 0
    val crossGen = ListBuffer[Char]()
    while (i < crossPoint) {
      if (gen(i) != '-') {
        i += 1
      }
      crossGen.addOne(gen(i))
    }
    crossGen.toList
  }

  /**
   * Return the last crossPoint characters skipping dashes
   * @param gen list of characters
   * @param crossPoint limit of characters for the new list
   * @return a list of characters up to crossPoint skipping dashes
   */
  def getLast(gen: List[Char], crossPoint: Int): List[Char] = {
    var i = crossPoint
    val crossGen = ListBuffer[Char]()
    while (i <= gen.length-1) {
      if (gen(i) != '-') {
        i += 1
      }
      crossGen.addOne(gen(i))
    }
    crossGen.toList
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
   * Calculates the minimum length of two lists ignoring dashes
   * @param gen1 first gen
   * @param gen2 second gen
   * @return the minimum length of two lists
   */
  def getMinLength(gen1: List[Char], gen2: List[Char]): Int = {
    val len1 = gen1.filterNot(c => c == '-').length
    val len2 = gen2.filterNot(c => c == '-').length
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
