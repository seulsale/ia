package algoritmogenetico

import scala.collection.mutable.ListBuffer
import scala.util.Random

class Population(val population: List[Backpack]) {
  population.foreach(_.totalFitness(getCalories, getWeight))

  /**
   * Returns the total calories of the population
   *
   * @return
   */
  def getCalories: Double = {
    var calories = 0.0
    for (individual <- population) {
      calories += DataTable.getTotalCalories(individual.items)
    }
    calories
  }

  /**
   * Returns the total weight of the population
   *
   * @return
   */
  def getWeight: Double = {
    var weight = 0.0
    for (individual <- population) {
      weight += DataTable.getTotalWeight(individual.items)
    }
    weight
  }

  /**
   * Returns the backpack that meet the constraints and has the highest calories, if it does not meet the constraints
   * returns the backpack with the highest calories
   *
   * @param maxWeight   maximum weight allowed
   * @param minCalories minimum calories allowed
   * @return
   */
  def getBest(maxWeight: Double, minCalories: Double): Option[Backpack] = {
    var afterConstraints = ListBuffer[Backpack]()

    population.foreach(backpack => {
      if (DataTable.getTotalWeight(backpack.items) < maxWeight
        && DataTable.getTotalCalories(backpack.items) > minCalories) afterConstraints += backpack
    })
    var bestIndex = 0
    var mostCalories = 0.0
    var result: Option[Backpack] = None
    if (afterConstraints.toList.nonEmpty) {
      afterConstraints.toList.zipWithIndex.foreach { case (backpack, index) =>
        val calories: Double = DataTable.getTotalCalories(backpack.items)
        if (calories > mostCalories) {
          bestIndex = index
          mostCalories = calories
        }
      }
      result = Some(afterConstraints(bestIndex))
    }
    result
  }

  override def toString: String = {
    var str = ""
    population.zipWithIndex.foreach({ case (item, index) =>
      str += f"\n#### Individual ${index + 1} ####${item.toString}"
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
      population += new Backpack(Seq.fill(chromosomes) {
        Random.between(0, 2)
      })
    }
    population.toList
  }

  def newPopulationByCrossing(population: List[Backpack], crossing: Int): Population = {
    val sortedByWeighing = population.sortBy(_.totalF)
    var newPopulation = new ListBuffer[Backpack]()
    var start = 0
    while (start < population.length) {
      newPopulation += new Backpack(sortedByWeighing(start).items.take(crossing) ++ sortedByWeighing(start + 1).items.drop(crossing))
      newPopulation += new Backpack(sortedByWeighing(start).items.drop(crossing) ++ sortedByWeighing(start + 1).items.take(crossing))
      start += 2
    }
    new Population(newPopulation.toList)
  }
}