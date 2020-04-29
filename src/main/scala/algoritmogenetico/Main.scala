package algoritmogenetico

import scala.util.Random

/**
 * This implementation tries to discover the backpack with the most calories and the lesser weight in {t} iterations.
 *
 */
object Main extends App {
  var population = new Population(Population.createPopulation(4, 8))
  println(f"Initial population: $population")

  var currentBest = population.getBest
  print(currentBest)

  var p = 0
  val limit = 50
  while (p < limit) {
    println(f"##### ${p+1}th iteration #####")

    val newBest = population.getBest

    // TODO: Implement comparison function, total fitness is used temporarily
    if (currentBest.totalF < newBest.totalF) {
      currentBest = newBest
    }

    population = Population.newPopulationByCrossing(population.population, Random.between(1, 7))

    p += 1
  }

  println(f"The best individual is $currentBest")
}
