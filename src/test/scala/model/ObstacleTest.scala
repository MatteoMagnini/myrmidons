package model

import org.scalatest._

import model.EnvObstacle
import TupleOp3._
class ObstacleTest  extends wordspec.AnyWordSpec {
  "An obstacle" when {
    "initialized" should {
      "produced IllegalArgumentException when initialized with 2 elements list" in {
        val points: List[Vector3D] = List((1,0,1), (3,2,1))
        assertThrows[IllegalArgumentException]{
          new EnvObstacle(points)
        }
      }

      "have a complex geometry" in {
        val points: List[Vector3D] = List((1,1,1), (2,1,1), (3.0,1.5,1.0), (2,2,1), (3.0,2.5,1.0), (1,3,1))
        val o = new EnvObstacle(points);

        assert(o.isInside((1.5,2.25,1.0)))
        assert(o.isInside((2.5,2.0,1.0)) === false)
      }
    }
  }

  "An point" when {
    "inside an obstacle " should {
      "return true" in {
        val points: List[Vector3D] = List((1, 1, 1), (2, 1, 1), (2, 2, 1), (1, 2, 1))
        val o = new EnvObstacle(points)
        assert(o.isInside((1.5, 1.5, 1.0)))
        }
      }
    }

  "outside an obstacle " should {
    "return false" in {
      val points: List[Vector3D] = List((1,1,1), (1,2,1), (2,2,1), (2,1,1))
      val o = new EnvObstacle(points)
      assert(o.isInside((2.5,0.5,1.0)) === false)
    }
  }
}
