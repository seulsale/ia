package algoritmogenetico

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Population(population: List[Backpack]) {
  def getCalories: Double = {
    var calories = 0.0
    for (individual <- population) {
      calories += DataTable.getTotalCalories(individual.items)
    }
    calories
  }

  def getWeight: Double = {
    var weight = 0.0
    for (individual <- population) {
      weight += DataTable.getTotalWeight(individual.items)
    }
    weight
  }

  def getBest: Backpack = {
    var bestIndex = 0
    var bestFitness = 0.0
    population.zipWithIndex.foreach { case (backpack, index) =>
      val fitness = backpack.totalFitness(getCalories, getWeight)
      if (fitness > bestFitness) {
        bestFitness = fitness
        bestIndex = index
      }
    }
    population(bestIndex)
  }

  override def toString: String = {
    var str = ""
    population.foreach(item => {
      str += f"${item.toString}\n"
    })
    str
  }
}

object Population {
  /**
   * Generates a List of length individuals containing sequences of length chromosomes containing values between 0 and 1.
   *
   * @param individuals the size of the population
   * @param chromosomes the quantity of chromosomes per individual
   */
  def createPopulation(individuals: Int, chromosomes: Int): List[Backpack] = {
    val population = new ListBuffer[Backpack]()
    for (individual <- 1 to individuals) {
      population += new Backpack(Seq.fill(chromosomes) { Random.between(0, 2) })
    }
    population.toList
  }

  def crossIndividuals(ind1: Backpack, ind2: Backpack, crossoverIndex: Int): (Backpack, Backpack) = {
    // implement
    (ind1, ind2)
  }
}