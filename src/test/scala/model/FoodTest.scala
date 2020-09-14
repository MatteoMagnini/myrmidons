package model

import org.scalatest.wordspec
import utility.Geometry.TupleOp._

class FoodTest extends wordspec.AnyWordSpec {
  "A food" when {
    val f = Food((10,15), 100)
    "create with 100 of quantity" should {
      "have 100 of disponibility" in {
        assert(f.quantity === 100)
      }
    }
    "increased of 50" should {
      val f2 = f + 50;
      "have 150 of disponibility" in {
        assert(f2.quantity === 150)
      }
    }
    "decrease of 50" should {
      val f3 = f - 50;
      "have 50 of disponibility" in {
        assert(f3.quantity === 50)
      }
    }
    "exist" should{
      "have a dimension that follow log(base 10) rounded to int" in {
        assert(f.xDim === math.log10(100))
      }
    }
  }
}
