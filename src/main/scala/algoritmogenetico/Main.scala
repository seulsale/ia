package algoritmogenetico

import scala.collection.mutable.ListBuffer
import scala.io.Source
import scala.util.Random

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

  def getScores(population: List[Seq[Int]]): List[Int] = {
    val scores = new ListBuffer[Int]()
    for (individual <- population) {

    }
    scores.toList
  }

  val population = generatePopulation(4, 8)

  println(f"Initial population: ${population}")
}
