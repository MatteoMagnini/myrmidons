package model

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.Vector3D

class Vector3Test extends AnyWordSpecLike with BeforeAndAfter {

  "A vector" when {
    import utility.Geometry.TupleOp._

    val v = (6, -4, 2)

    "calculate its opposite" should {
      "give the correct result" in {
        assert((v -) == Vector3D(-6, 4, -2))
      }
    }
    "calculated in module" should {
      "give the correct result" in {
        assert((v ||) == 2 * Math.sqrt(14))
      }
    }
    "multiplied by a constant" should {
      "give the correct result" in {
        assert((v * 3) == Vector3D(18, -12, 6))
      }
    }
    "divided by a constant" should {
      "give the correct result" in {
        assert((v / 2) == Vector3D(3, -2, 1))
      }
    }
  }

  "Two vectors" when {
    import utility.Geometry.TupleOp._

    val v1 = (0, 0, 0)
    val v2 = (3, -4, 1)

    "added" should {
      "give the correct result" in {
        assert((v1 >> v2) == implicitly[Vector3D](v2))
      }
    }

    "subtracted" should {
      "give the correct result" in {
        assert(v1 - v2 == (v2 -))
      }
    }

    "calculated in their distance" should {
      "give the correct result" in {
        assert((v1 --> v2) == Math.sqrt(26))
      }
    }

    "calculate cross product" should {
      "give the correct result " in {
        assert((v1 X v2) === Vector3D(0,0,0))
      }
    }
  }
}

