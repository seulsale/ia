package algoritmogenetico

class Backpack(val items: Seq[Int], var calF: Double = 0, var weiF: Double = 0) {
  def caloriesFitness(totalCalories: Double): Double = {
    calF = DataTable.getTotalCalories(items) / totalCalories
    calF
  }

  def weightFitness(totalWeight: Double): Double = {
    weiF = DataTable.getTotalWeight(items) / totalWeight
    weiF
  }

  def totalFitness(totalCalories: Double, totalWeight: Double): Double = {
    caloriesFitness(totalCalories) + weightFitness(totalWeight)
  }

  override def toString: String = {
    f"""
       |$items
       |weightFitness: $weiF
       |caloriesFitness: $calF
       |calories: ${DataTable.getTotalCalories(items)}
       |weight: ${DataTable.getTotalWeight(items)}
       |"""".stripMargin
  }
}
