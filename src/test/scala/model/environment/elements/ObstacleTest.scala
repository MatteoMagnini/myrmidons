package model.environment.elements

import org.scalatest._
import utility.geometry.TupleOp2._
import utility.geometry.{Vector2D, Vectors}

class ObstacleTest  extends wordspec.AnyWordSpec {

  "An obstacle" when {
    "created " must {
      "have 3 or more vertex" in {
        assertThrows[IllegalArgumentException](Obstacle((0,0), nSides = 2))
        val o = Obstacle((0,0), nSides = 3)
        assert(o.points.size == 3)
      }
    }

    "have a regular triangle form" should {
      val o1 = Obstacle.Triangle((0,0))
      assert(o1.points.size == 3)

      "have similar angle between two vertex" in {
        val angle01 = o1.points.head ^ o1.points(1)
        val angle12 = o1.points(1) ^ o1.points(2)
        val angle20 = o1.points(2) ^ o1.points.head
        assert(Vectors.~=(angle01, angle12, 1E-7))
        assert(Vectors.~=(angle12, angle20, 1E-7))
        assert(Vectors.~=(angle20, angle01, 1E-7))
      }

      "have segment with similar length" in {
        val length01 = o1.points.head --> o1.points(1)
        val length12 = o1.points(1) --> o1.points(2)
        val length20 = o1.points(2) --> o1.points.head
        assert(Vectors.~=(length01, length12, 1E-7))
        assert(Vectors.~=(length12, length20, 1E-7))
        assert(Vectors.~=(length20, length01, 1E-7))
      }

      "have position at equal distance from the vertex" in {
        val dist0 = o1.position --> o1.points.head
        val dist1 = o1.position --> o1.points(1)
        val dist2 = o1.position --> o1.points(2)
        assert(Vectors.~=(dist0, dist1, 1E-7))
        assert(Vectors.~=(dist1, dist2, 1E-7))
        assert(Vectors.~=(dist2, dist0, 1E-7))
      }
    }
  }

  "An obstacle" can {

    "check intersection point and angle" when {
      val externalPoint:Vector2D = (6,6)
      val obstacleCenter:Vector2D = (0,0)
      val externalPoint2:Vector2D = (7,7)
      val internalPoint1:Vector2D = (0,1)
      val internalPoint2:Vector2D = (1,0)
      val throughPoint: Vector2D = (-10, -10)
      val o1 = Obstacle.Square(obstacleCenter)

      "intersection exist and " should {

        "have rect angle " in {
          val i = o1.findIntersectionInformation(externalPoint, obstacleCenter)
          assert(i.isDefined)
          assert(i.head.angle == math.Pi / 2 )
        }
        "have acute angle " in {
          val i = o1.findIntersectionInformation(externalPoint, internalPoint1)
          assert(i.isDefined)
          assert(i.head.angle < math.Pi / 2 )
        }

        "have obtuse angle "  in {
          val i = o1.findIntersectionInformation(externalPoint, internalPoint2)
          assert(i.isDefined)
          assert(i.head.angle > math.Pi / 2 )
        }

        "bounce if path pass through obstacle" in {
          val i = o1.findIntersectionInformation(externalPoint, throughPoint)
          assert(i.isDefined)
        }
      }

      "intersection doesn't exist" in {
        val i = o1.findIntersectionInformation(externalPoint, externalPoint2)
        assert(i.isEmpty)
      }

    }

  }

  "two obstacle" when {
    val o1 = Obstacle.Square((0,0))
    val o2 = Obstacle.Square((0,4))
    val o3 = Obstacle.Square((0,10))

    "are overlapped" should{

      "join in a single obstacle" in {
        val outObstacle = o1 >< o2
        assert(outObstacle.isDefined)
        assert(outObstacle.head.points.size > o1.points.size)
      }
    }

    "aren't overlapped" should {
      "return Option.empty" in {
        val outObstacle = o1 >< o3
        assert(outObstacle.isEmpty)
      }
    }

    "are equals" in {
      assert(o1.equals(Obstacle.Square((0,0))))
      assert(!o1.equals(Obstacle.Triangle((0,0))))
    }

    "are different" in {
      assert(!o1.equals(o2))
      assert(!o1.equals(Obstacle.Triangle((0,0))))
    }
  }

  "an obstacle and a food" should {
    val o1 = Obstacle.Square((0,0))
    val food = Food((4,4),10, Obstacle((4,4),Food.radius(10),16))
    "not join" in {
      assertThrows[IllegalArgumentException](o1 >< food)
    }
  }

  "Random creation " should {
    "create obstacle and return joined obstacle" in {
      val ol = Obstacle.createRandom(20, (0,0), (50,100))
      assert(ol.size <= 20)
    }
  }
}
