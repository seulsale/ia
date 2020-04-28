package algoritmogenetico

import scala.io.Source
import scala.util.Random
import scala.collection.mutable.ListBuffer

object Main extends App {

  /**
   * Generates a List of length individuals containing sequences of length chromosomes containing values between 0 and 1.
   *
   * @param individuals the size of the population
   * @param chromosomes the quantity of chromosomes per individual
   */
  def generatePopulation(individuals: Int, chromosomes: Int): List[Seq[Int]] = {
    val population = new ListBuffer[Seq[Int]]()
    for (individual <- 1 to individuals) {
      population += Seq.fill(chromosomes) {
        Random.between(0, 2)
      }
    }
    population.toList
  }

  val data = Source.fromResource("datos.csv").getLines().drop(1).map(_.split(",")) // Get table containing calories and weight values from csv file

  val population = generatePopulation(4, 8)

  for (item <- data) {
    item foreach println
  }
  println(population)
}
