package algoritmogenetico

class Backpack(val items: Seq[Int], var calF: Double = 0.0, var weiF: Double = 0.0, var totalF: Double = 0.0) {
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
