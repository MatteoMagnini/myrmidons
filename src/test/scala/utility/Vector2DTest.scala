package utility

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike

class Vector2DTest extends AnyWordSpecLike with BeforeAndAfter {

  import utility.Geometry._
  import utility.Geometry.TupleOp._

  "A vector" when {

    val v = (3, -4)

    "calculating its opposite" should {
      "give the correct result" in {
        assert((v -) == Vector2D(-3, 4))
      }
    }
    "calculating module" should {
      "give the correct result" in {
        assert((v ||) == 5)
      }
    }
    "multiplied by a constant" should {
      "give the correct result" in {
        assert((v * 3) == Vector2D(9, -12))
      }
    }
    "calculating direction" should {
      "give the correct result" in {
        println(v /\)
        assert((v /\) == math.atan(v.y/v.x))
      }
    }
  }

  "Two vectors" when {

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

  "A random vector" when {

    "created in range" should {
      val min = 0.3
      val max = 1.4
      val r = RandomVector2D(min,max)

      "have its dimensions inside the range" in {
        assert(r.x.abs >= min && r.x.abs <= max && r.y.abs >= min && r.y.abs <= max )
      }
    }

    "created in range with a perturbation" should {

      val min = 0.3
      val max = 1.4
      val perturbation = (-0.5,0.93)
      val r = RandomVector2D(min,max,perturbation)

      "have its dimensions inside the range" in {
        assert(r.x.abs >= min && r.x.abs <= max && r.y.abs >= min && r.y.abs <= max )
      }
    }

  }
}
