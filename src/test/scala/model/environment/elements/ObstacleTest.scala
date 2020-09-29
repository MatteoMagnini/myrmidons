package model.environment.elements

import org.scalatest._
import utility.geometry.TupleOp2._
import utility.geometry.TupleOp3._
import utility.geometry.Vector3D

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
        val o = Obstacle(points)

        import model.environment.elements.EnvironmentElements._
        assert(checkHasInside(o, (1.5,2.25)))
        assert(checkHasInside(o, (2.5,2.0)) === false)
      }
    }
  }

 /* "A SimpleObstacle" when {
    val o = new Obstacle(Vector2D(50.0, 10.0), 20, 10)
    "has inside a point" should {
      "return true" in {
        assert(o hasInside Vector2D(50,10))
      }
      "the path intersect" in {
        val start = Vector2D(50, 17)
        val stop = Vector2D(50, 10)
        assert(o hasInside(stop))
        val res = o findIntersectionPoint(start, stop)
        assert(res.get.intersectionPoint === Vector2D(50, 15))
        val angle = (res.get.angle * 100).round / 100.toDouble
        val test = ( Math.PI/2 * 100).round / 100.toDouble
        assert(angle == test)
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

  "a path that no have intersection with a obstacle" must {
    "return Option.empty" in{
      val o = new Obstacle(Vector2D(50.0, 10.0), 20, 10)
      val start = Vector2D(50, 17)
      val stop = Vector2D(50, 16)
      val res = o findIntersectionPoint(start, stop)
      assert(res === Option.empty)
    }
  }*/
}
