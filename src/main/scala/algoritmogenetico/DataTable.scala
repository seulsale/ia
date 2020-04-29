package algoritmogenetico

import scala.io.Source

object DataTable {
  val data: Array[Array[Double]] = Source.fromResource("datos.csv").getLines().drop(1).map(_.split(",").map(_.toDouble).toArray).toArray // Get table containing calories and weight values from csv file

  def getCalories(index: Int): Double = {
    data(index)(1)
  }

  def getWeight(index: Int): Double = {
    data(index)(2)
  }

  def getTotalWeight: Double = {
    val weights = data.map(_(2))
    weights.sum
  }

  def getTotalCalories: Double = {
    val calories = data.map(_(1))
    calories.sum
  }
}