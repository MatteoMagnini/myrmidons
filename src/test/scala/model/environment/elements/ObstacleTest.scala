package model.environment.elements

import org.scalatest._
import utility.geometry.TupleOp2._
import utility.geometry.Vector2D

class ObstacleTest  extends wordspec.AnyWordSpec {
  "An obstacle" when {
    "initialized" should {
      "produced IllegalArgumentException when initialized with 2 elements list" in {
        val points: List[Vector2D] = List((1,0), (3,2))
        assertThrows[IllegalArgumentException]{
          Obstacle(points)
        }
      }

      "have a complex geometry" in {
        val points: List[Vector2D] = List((1,1), (2,1), (3.0,1.5), (2,2), (3.0,2.5), (1,3))
        val o = Obstacle(points)

        import model.environment.elements.EnvironmentElements._
        assert(checkHasInside(o, (1.5,2.25)))
        assert(checkHasInside(o, (2.5,2.0)) === false)
      }
    }
  }
  
}
