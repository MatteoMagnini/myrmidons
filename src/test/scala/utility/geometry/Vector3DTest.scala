package utility.geometry

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.TupleOp3._
class Vector3DTest extends AnyWordSpecLike with BeforeAndAfter {

  "A vector" when {
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

   /* "aaaa" in {
      val o = new Obstacle(Vector2D(4.5,4.5), 5, 5)
      val t1 = Vector2D(2,1)
      val t2 = Vector2D(4,4)

      val test  = o.findIntersectionPoint(t1,t2)

      assert(test.get.angle < math.Pi/2)

      val t3 = Vector2D(7,1)
      val test2 = o.findIntersectionPoint(t3,t2)
      assert(test2.get.angle > math.Pi/2)
    }*/

  }
}

