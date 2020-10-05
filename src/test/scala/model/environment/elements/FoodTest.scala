package model.environment.elements

import org.scalatest.wordspec
import utility.geometry.TupleOp2._

class FoodTest extends wordspec.AnyWordSpec {
  "A food" when {
    val f = Food((10,15), 100, Obstacle((10,15),if (math.sqrt(100) < 5) 5 else math.sqrt(100), 16))
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
        //assert(f.xDim === 20)
      }
    }
  }
}
