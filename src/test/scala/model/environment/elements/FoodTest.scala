package model.environment.elements

import org.scalatest.wordspec
import utility.geometry.Vectors

class FoodTest extends wordspec.AnyWordSpec {
  "A food" when {
    val f = Food((10,15), 100, Obstacle((10,15),Food.radius(100), 16))
    "create with 100 of quantity" should {
      "have 100 of availability" in {
        assert(f.quantity === 100)
      }
    }
    "increased of 50" should {
      val f2 = f + 50
      "have 150 of availability" in {
        assert(f2.quantity === 150)
      }
    }
    "decrease of 50" should {
      val f3 = f - 50
      "have 50 of availability" in {
        assert(f3.quantity === 50)
      }
    }
    "exist" should {
      "have a dimension that follow sqrt(100)" in {
        assert(Vectors.~=(f.position-->f.points.head, f.radius, 1E-6))
      }
    }
  }
}
