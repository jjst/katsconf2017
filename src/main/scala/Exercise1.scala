package katsconf

import cats.data._
import cats.implicits._
import cats._, cats.data.Validated, cats.instances.all._
import cats.syntax.cartesian._

object Exercise1 {

  sealed trait Failure
  case object EmptyName extends Failure
  case object InvalidSalary extends Failure

  case class Employee private[Exercise1](name: String, zipCode: String, city: String, salary: Int)

  // Task 1: You are given raw employee data. If there's an error, give it back.
  // You'll need to add cases to the `Failure` trait.

  /**
    * @param name must be non-empty
    * @param city must be in the set of cities belonging to the `zipCode`, according to `ZipCodeService`
    * @param salary must be greater than 0 and less than 100000
    */
  def mkEmployee(name: String, zipCode: String, city: String, salary: Int): Either[Failure, Employee] = {
    if (name.isEmpty) {
      Left(EmptyName)
    } else if (salary < 0 || salary > 10000) {
      Left(InvalidSalary)
    } else {
      Right(Employee(name, zipCode, city, salary))
    }
  }

  // Task 2: You are given raw employee data. If there's at least one error, return all existing errors back.
  // Don't use Validated#andThen.

  def mkEmployee2(name: String, zipCode: String, city: String, salary: Int): ValidatedNel[Failure, Employee] = {
    def invalid(f: Failure) = Validated.invalid[NonEmptyList[Failure], Employee](NonEmptyList.of(EmptyName))
    val employee = Validated.valid[NonEmptyList[Failure], Employee](Employee(name, zipCode, city, salary))
    val e1 = if (name.isEmpty) invalid(EmptyName) else employee
    val e2 = if (salary < 0 || salary > 10000) invalid(InvalidSalary) else employee
    // (e1 |@| e2) map { case (x,y) => ??? }
    ???
  }

  // Task 3: You have to fetch the salary data from the `SalaryService`.
  // You'll need to add even more cases to the `Failure` trait.
  // Don't use Validated#andThen.

  def mkEmployee3(name: String, zipCode: String, city: String): ValidatedNel[Failure, Employee] =
    ???

}
