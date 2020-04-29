package algoritmogenetico

import scala.util.Random

/**
 * This implementation tries to discover the backpack with the most calories and the lesser weight in {t} iterations.
 *
 */
object Main extends App {
  val initialPopulation = new Population(Population.createPopulation(4, 8))
  println(f"Initial population: $initialPopulation")

  var currentBest = initialPopulation.getBest
  print(currentBest)
}
