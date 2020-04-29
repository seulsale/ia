package algoritmogenetico

import org.scalatest.FunSuite

class DataTableTest extends FunSuite{
  test("Sum weight of individual") {
    assert(DataTable.getTotalWeight(Seq(0, 1, 0, 1, 1, 1, 1, 0)) == 2.1)
  }
  test("Sum calories of individual") {
    assert(DataTable.getTotalCalories(Seq(0, 1, 1, 1, 0, 0, 0, 0)) == 1200.0)
  }
  test("Get weight from index") {
    assert(DataTable.getWeight(5) == 0.4)
  }
  test("Get calories from index") {
    assert(DataTable.getCalories(2) == 400.0)
  }
}
