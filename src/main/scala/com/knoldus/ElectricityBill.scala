import scala.util.{Failure, Try}

case class CustomerDetails(accountNumber: Int,
                           username: String,
                           address: String,
                           previousUnits: Int,
                           currentUnits: Int)

object ElectricityBillCalculator {
  private val GST_RATE = 0.18

  // Calculates the electricity bill for a customer, including GST, and returns it as a Try[Double]
  def calculateBill(customer: CustomerDetails): Try[Double] = {
    Try {
      val totalUnits = customer.currentUnits - customer.previousUnits
      // Throws an exception if totalUnits is negative
      if (totalUnits < 0) throw new IllegalArgumentException("Invalid input: total units cannot be negative")
      val totalAmount = calculateTotalAmount(totalUnits)
      addGst(totalAmount)
    }.recoverWith {
      // Handles division by zero exceptions
      case _: ArithmeticException =>
        Failure(new ArithmeticException("Division by zero"))
      // Handles any other exceptions and wraps them in an appropriate exception type
      case e: IllegalArgumentException =>
        Failure(new IllegalArgumentException("Invalid input: " + e.getMessage))
      case e: Exception =>
        Failure(new Exception("An unexpected error occurred: " + e.getMessage))
    }
  }

  // Calculates the total amount to be paid for a given number of units
  def calculateTotalAmount(units: Int): Double = {
    val pricePerUnit = units match {
      case unit if unit <= 250 => 5.25
      case unit if unit <= 450 => 6.75
      case _ => 8.50
    }
    units * pricePerUnit
  }

  // Calculates the GST to be paid on an amount
  def addGst(amount: Double): Double = amount * GST_RATE
}
