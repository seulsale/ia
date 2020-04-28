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
  def generatePopulation(individuals: Int, chromosomes: Int): Unit = {
    val population = new ListBuffer[Seq[Int]]()
    for (individual <- 1 to individuals) {
      population += Seq.fill(chromosomes) {
        Random.between(0, 2)
      }
    }
    val populationList = population.toList
    populationList foreach println
  }

  val data = Source.fromFile("C:\\Users\\sergi\\IdeaProjects\\com.seulsale.ia\\src\\main\\scala\\algoritmogenetico\\datos.csv")
  val iter = data.getLines().drop(1).map(_.split(",")) // Get table containing calories and weight values from csv file

  generatePopulation(4, 8)

  data.close()
}
