package algoritmogenetico

import scala.util.Random

/**
 * This implementation tries to discover the backpack with the most calories and the lesser weight in {t} iterations.
 *
 */
object Main extends App {
  var population = new Population(Population.createPopulation(4, 8))
  println(f"Initial population: \n$population")

  var currentBest: Option[Knapsack] = population.getBest(2.0, 1500.0) match {
    case Some(backpack) => Some(backpack)
    case None =>
      println("Not individual in the current population meets te given requirements. Try again.")
      None
  }
  if (currentBest.isEmpty) sys.exit()
  print(f"Initial best: $currentBest")

  var p = 0
  val limit = 50
  while (p < limit) {
    println(f"##### ${p+1}th iteration #####")

    val newBest: Option[Knapsack] = population.getBest(2.0, 1500.0)

    if (newBest.nonEmpty) {
      if (DataTable.getTotalCalories(newBest.get.items) > DataTable.getTotalCalories(currentBest.get.items)) currentBest = Some(newBest.get)
    }

    population = Population.newPopulationByCrossing(population.population, Random.between(1, 7))

    p += 1
  }

  println(f"The best individual is ${currentBest.get}")
}
