import org.scalatest.funsuite.AnyFunSuite
import scala.util.Success

class ElectricityBillCalculatorTest extends AnyFunSuite {
  test("calculateTotalAmount should return correct amount for units less than or equal to 250") {
    assert(ElectricityBillCalculator.calculateTotalAmount(200) == 1050.0)
  }

  test("calculateTotalAmount should return correct amount for units between 251 and 450") {
    assert(ElectricityBillCalculator.calculateTotalAmount(300) == 2025.0)
  }

  test("calculateTotalAmount should return correct amount for units greater than 450") {
    assert(ElectricityBillCalculator.calculateTotalAmount(600) == 5100.0)
  }

  test("addGst should add GST to given amount") {
    assert(ElectricityBillCalculator.addGst(1000.0) == 180.0)
  }

  test("calculateBill should return Success with correct amount for valid customer details") {
    val customer = CustomerDetails(1234, "Krishna", "Krishna Nagar", 500, 800)
    assert(ElectricityBillCalculator.calculateBill(customer) == Success(364.5))
  }

  test("calculateBill should return Failure for invalid customer details with negative units") {
    val customer = CustomerDetails(2345, "Asbin", "Assam", 800, 500)
    assert(ElectricityBillCalculator.calculateBill(customer).isFailure)
  }
}
