package model

import org.scalatest._

import model.EnvObstacle

class ObstacleTest  extends wordspec.AnyWordSpec {

  "An obstacle" when {
    "initialized" should {
      "produced IllegalArgumentException when initializzated with 2 elemens list" in {
        val points: List[(Double, Double, Double)] = List((1,0,1), (3,2,1))
        assertThrows[IllegalArgumentException]{
          new EnvObstacle(points)
        }
        "produced IllegalArgumentException when initializzated with 2 elemens list" in {
          val points: List[(Double, Double, Double)] = List((1,0,1))
          assertThrows[IllegalArgumentException]{
            new EnvObstacle(points)
          }
        }

      }
    }
  }
}
