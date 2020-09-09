package utility

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike

class VectorsTest extends AnyWordSpecLike with BeforeAndAfter {

  import utility.Geometry._
  import utility.Geometry.TupleOp._

  "A vector" when {

    val v = (3, -4)

    "calculate its opposite" should {
      "give the correct result" in {
        assert((v -) == Vector2D(-3, 4))
      }
    }
    "calculated in module" should {
      "give the correct result" in {
        assert((v ||) == 5)
      }
    }
    "multiplied by a constant" should {
      "give the correct result" in {
        assert((v * 3) == Vector2D(9, -12))
      }
    }
  }

  "Two vectors" when {

    val v1 = (0, 0)
    val v2 = (3, -4)

    "added" should {
      "give the correct result" in {
        assert((v1 >> v2) == implicitly[Vector](v2))
      }
    }

    "subtracted" should {
      "give the correct result" in {
        assert(v1 - v2 == (v2 -))
      }
    }

    "calculated in their distance" should {
      "give the correct result" in {
        assert((v1 --> v2) == 5)
      }
    }
  }
}
