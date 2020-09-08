package utility

import org.scalatest.wordspec.AnyWordSpecLike
import org.junit.Test
import org.junit.Assert._
import utility.Geometry._

class GeometryTest extends AnyWordSpecLike {

  @Test def verifyMod(): Unit = {
    val v1: Vector = Vector2D(0,0)
    val v2: Vector = Vector2D(3,-4)
    assertTrue(v1.mod == 0)
    assertTrue(v2.mod == 5)
  }

  @Test def verifySum(): Unit = {
    val v1: Vector = Vector2D(10,20)
    val v2: Vector = Vector2D(5,10)
    assertTrue(v1 + v2 == Vector2D(15,30))
  }

  @Test def verifySubtraction(): Unit = {
    val v1: Vector = Vector2D(10, 5)
    val v2: Vector = Vector2D(7, 5)
    assertTrue(v1 - v2 == Vector2D(3,0))
  }

  @Test def verifyDistance(): Unit = {
    val v1: Vector = Vector2D(2,-2)
    val v2: Vector = Vector2D(-1,2)
    assertTrue((v1 dist v2) == 5 )
  }

}
