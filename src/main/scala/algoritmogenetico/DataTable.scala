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

  def getTotalWeight(items: Seq[Int]): Double = {
    var weight = 0.0
    items.zipWithIndex.foreach { case (item, index) =>
      if (item == 1) weight += DataTable.getWeight(index)
    }
    weight
  }

  def getTotalCalories(items: Seq[Int]): Double = {
    var calories = 0.0
    items.zipWithIndex.foreach { case (item, index) =>
      if (item == 1) calories += DataTable.getCalories(index)
    }
    calories
  }
}