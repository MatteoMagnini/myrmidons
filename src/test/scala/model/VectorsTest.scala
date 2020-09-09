package model

import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.BeforeAndAfter


class VectorsTest extends AnyWordSpecLike with BeforeAndAfter {

  "A vector" when {
    import TupleOp._

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
    import TupleOp._

    val v1 = (0, 0)
    val v2 = (3, -4)

    "added" should {
      "give the correct result" in {
        assert((v1 >> v2) == implicitly[Vector2D](v2))
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
