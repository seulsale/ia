package alineaciondesecuencias

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

object Main extends App {
  var human = Source.fromResource("secuencias/humano/env - HIV1H.txt").toList // .split("")
  var chimpanzee = Source.fromResource("secuencias/chimpance/env - HIV1S.txt").toList

  var population: List[(List[Char], List[Char])] = Population.createPopulation(human, chimpanzee, 80)

  var bestFitness = Population.getSimilarity(population.head._1, population.head._2)
  var bestPopulation = population.head

  var limit = 20
  var p = 1
  while (p < limit) {
    for ((ind, index) <- population.zipWithIndex) {
      val currentFitness = Population.getSimilarity(ind._1, ind._2)
      if (currentFitness > bestFitness) {
        bestFitness = currentFitness
        bestPopulation = population(index)
      }
      println(f"---------Fitness actual: $currentFitness---------")
    }

    population = Population.crossPopulation(population)

    println("---------Nueva generación---------")

    p += 1
  }

  println(f"Best Fitness: $bestFitness")
  println(f"Best Population: $bestPopulation")
}

object Population {
  def createPopulation(gen1: List[Char], gen2: List[Char], quantity: Int): List[(List[Char], List[Char])] = {
    val population = ListBuffer[(List[Char], List[Char])]()
    for (i <- 1 to quantity) { // for i in range(1, quantity)
      val newInd = (mutation(gen1, 2), mutation(gen2, 2))
      population.addOne(newInd)
    }
    population.toList
  }

  /**
   * Generates a new population
   *
   * @param population list of individuals
   * @return a new population which individuals may or may not be mutated
   */
  def crossPopulation(population: List[(List[Char], List[Char])]): List[(List[Char], List[Char])] = {
    val newPopulation = ListBuffer[(List[Char], List[Char])]()
    var start = 0
    while (start < population.length) {
      val crossing = Random.between(0, getMinLength(population(start)._1, population(start + 1)._1))

      newPopulation.addOne((getFirst(population(start)._1, crossing) ++ getLast(population(start + 1)._1, crossing),
        getFirst(population(start)._2, crossing) ++ getLast(population(start + 1)._2, crossing)))
      newPopulation.addOne((getLast(population(start)._1, crossing) ++ getFirst(population(start + 1)._1, crossing),
        getLast(population(start)._2, crossing) ++ getFirst(population(start + 1)._2, crossing)))

      start += 2
    }

    newPopulation.toList
  }

  /**
   * Return the first crossPoint characters skipping dashes
   *
   * @param gen        list of characters
   * @param crossPoint limit of characters for the new list
   * @return a list of characters up to crossPoint skipping dashes
   */
  def getFirst(gen: List[Char], crossPoint: Int): List[Char] = {
    var index = 0
    var char = 0
    val crossGen = ListBuffer[Char]()

    gen.take(crossPoint)
  }

  /**
   * Return the last crossPoint characters skipping dashes
   *
   * @param gen        list of characters
   * @param crossPoint limit of characters for the new list
   * @return a list of characters up to crossPoint skipping dashes
   */
  def getLast(gen: List[Char], crossPoint: Int): List[Char] = {
    var i = gen.length - 1
    val crossGen = ListBuffer[Char]()
//    while (i >= crossPoint) {
//      if (gen(i) != '-') {
//        i -= 1
//      }
//      crossGen.addOne(gen(i))
//    }
    gen.drop(crossPoint)
  }

  /**
   * Add dashes between the list of chars, the quantity of dashes depends on the percentage given
   *
   * @param gen        list of chars
   * @param percentage percentage number
   * @return a list containing random dashes in between
   */
  def mutation(gen: List[Char], percentage: Int): List[Char] = {
    val mutations = Math.round(gen.length / 100 * percentage.toFloat)
    var newGen: List[Char] = gen.map(c => c)
    for (i <- 1 to mutations) {
      val index = Random.between(0, newGen.length)
      if (newGen(index) == '-') {
        newGen = newGen.take(index) ++ newGen.drop(index + 1) // if current index is a dash, remove it
      } else {
        newGen = newGen.take(index) ++ ('-' :: newGen.drop(index)) // else add a dash at current index
      }
    }
    newGen
  }

  /**
   * Calculates the minimum length of two lists ignoring dashes
   *
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
   *
   * @param gen1 List of characters of the first gen
   * @param gen2 List of characters of the second gen
   * @return the percentage of similarity ranging from 0 to 1
   */
  def getSimilarity(gen1: List[Char], gen2: List[Char]): Double = {
    val maxLimit = getMinLength(gen1, gen2)
    var index: Int = 0
    var same: Int = 0
    while (index < maxLimit) {
      if (gen1(index) != '-' && gen2(index) != '-') {
        if (gen1(index) == gen2(index)) same += 1
      }
      index += 1
    }
    same.toFloat / maxLimit
  }
}
