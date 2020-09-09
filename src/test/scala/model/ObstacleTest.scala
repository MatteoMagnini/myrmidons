package model

import org.scalatest._

import model.EnvObstacle

class ObstacleTest  extends wordspec.AnyWordSpec {

  "An obstacle" when {
    "initialized" should {
      "produced IllegalArgumentException when initialized with 2 elements list" in {
        val points: List[(Double, Double, Double)] = List((1,0,1), (3,2,1))
        assertThrows[IllegalArgumentException]{
          new EnvObstacle(points)
        }
      }

      "have a complex geometry" in {
        val points: List[(Double, Double, Double)] = List((1,1,1), (2,1,1), (3,1.5,1), (2,2,1), (3,2.5,1), (1,3,1))
        val o = new EnvObstacle(points);

        assert(o.isInside((1.5,2.25,1)))
        assert(o.isInside((2.5,2,1)) === false)
      }
    }
  }

  "An point" when {
    "inside an obstacle " should {
      "return true" in {
        val points: List[(Double, Double, Double)] = List((1, 1, 1), (2, 1, 1), (2, 2, 1), (1, 2, 1))
        val o = new EnvObstacle(points)
        assert(o.isInside((1.5, 1.5, 1)))
        }
      }
    }

  "outside an obstacle " should {
    "return false" in {
      val points: List[(Double, Double, Double)] = List((1,1,1), (1,2,1), (2,2,1), (2,1,1))
      val o = new EnvObstacle(points)
      assert(o.isInside((2.5,0.5,1)) === false)
    }
  }
}
