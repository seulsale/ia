package algoritmogenetico

import scala.util.Random

class Knapsack(var items: Seq[Int], var calF: Double = 0.0, var weiF: Double = 0.0, var totalF: Double = 0.0) {
  mutation()

  def mutation(): Unit = {
    val index = Random.between(0, items.length-1)
    if (items(index) == 0) items.updated(index, 1)
    if (items(index) == 1) items.updated(index, 0)
  }

  def caloriesFitness(totalCalories: Double): Double = {
    calF = DataTable.getTotalCalories(items) / totalCalories
    calF
  }

  def weightFitness(totalWeight: Double): Double = {
    weiF = DataTable.getTotalWeight(items) / totalWeight
    weiF
  }

  def totalFitness(totalCalories: Double, totalWeight: Double): Double = {
    totalF = caloriesFitness(totalCalories) + weightFitness(totalWeight)
    totalF
  }

  override def toString: String = {
    f"""
       |$items
       |weight: ${DataTable.getTotalWeight(items)}
       |calories: ${DataTable.getTotalCalories(items)}
       |weightFitness: $weiF
       |caloriesFitness: $calF
       |totalFitness: $totalF
       |"""".stripMargin
  }
}
