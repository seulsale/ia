package algoritmogenetico

import org.scalatest.FunSuite

class DataTableTest extends FunSuite{
  test("Sum weights of table") {
    assert(DataTable.getTotalWeight == 2.9)
  }
  test("Sum calories of table") {
    assert(DataTable.getTotalCalories == 2400.0)
  }
  test("Get weight from index") {
    assert(DataTable.getWeight(5) == 0.4)
  }
  test("Get calories from index") {
    assert(DataTable.getCalories(2) == 400.0)
  }
}
