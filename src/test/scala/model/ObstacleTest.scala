package model

import org.scalatest._
import utility.Geometry.{Vector2D, Vector3D}
import utility.Geometry.TupleOp3._
import utility.Geometry.TupleOp._

class ObstacleTest  extends wordspec.AnyWordSpec {
  "An obstacle" when {
    "initialized" should {
      "produced IllegalArgumentException when initialized with 2 elements list" in {
        val points: List[Vector3D] = List((1,0,1), (3,2,1))
        assertThrows[IllegalArgumentException]{
          Obstacle(points)
        }
      }

      "have a complex geometry" in {
        val points: List[Vector3D] = List((1,1,1), (2,1,1), (3.0,1.5,1.0), (2,2,1), (3.0,2.5,1.0), (1,3,1))
        val o = Obstacle(points);

        assert(o.hasInside((1.5,2.25)))
        assert(o.hasInside((2.5,2.0)) === false)
      }
    }
  }

  "A SimpleObstacle" when {
    val o = new SimpleObstacle(Vector2D(50.0, 10.0), 20, 10)
    "has inside a point" should {
      "return true" in {
        assert(o hasInside Vector2D(50,10))
      }
    }
    "has outside a point" should {
      "return false" in{
        assert((o hasInside Vector2D(100,10)) === false)
      }
    }
  }

  "An point" when {
    "inside an obstacle " should {
      "return true" in {
        val points: List[Vector3D] = List((1, 1, 1), (2, 1, 1), (2, 2, 1), (1, 2, 1))
        val o = Obstacle(points)
        assert(o.hasInside((1.5, 1.5)))
        }
      }
    }

  "outside an obstacle " should {
    "return false" in {
      val points: List[Vector3D] = List((1,1,1), (1,2,1), (2,2,1), (2,1,1))
      val o = Obstacle(points)
      assert(o.hasInside((2.5,0.5)) === false)
    }
  }
}
