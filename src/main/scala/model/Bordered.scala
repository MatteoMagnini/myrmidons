package model

import scala.language.implicitConversions
import utility.Geometry.{RandomVector2D, Vector2D}

/**
 * trait for a entity that present a border
 * */
trait Bordered extends Drawable {
  /**
   * function to verify if an entity has inside itself an
   * position.
   *
   * @param coordinate to check
   *
   * @return true if coordinate is inside of the obstacle
   * */
  def hasInside(coordinate: Vector2D):Boolean
}

object BorderedEntityFactory {
  /**
   * Create and instance of simple obstacle in a random position
   * with dimension xDim on x-axis and yDim on y-axis.
   *
   * @param minPos lowest value for position (both x and y coordinate)
   * @param maxPos highest value for position (both x and y coordinate)
   * @param xDim dimension on x-axis
   * @param yDim dimension on y-axis
   *
   * @return a SimpleObstacle instance
   * */
  def createRandomSimpleObstacle(minPos: Double = 0, maxPos: Double = 800, xDim: Int = 30, yDim: Int = 30):SimpleObstacle = {
    val pos = RandomVector2D(minPos, maxPos)
    new SimpleObstacle(Vector2D(pos.x, pos.y), xDim, yDim)
  }


  /**
   * Create and instance of food obstacle in a random position
   * and given quantity of food
   *
   * @param minPos lowest value for position (both x and y coordinate)
   * @param maxPos highest value for position (both x and y coordinate)
   * @param quantity of food of this resource
   *
   * */
  def createRandomFood(minPos: Double = 0, maxPos: Double = 800, quantity: Int = 1000):Food = {
    val pos = RandomVector2D(minPos, maxPos)
    Food(Vector2D(pos.x, pos.y), quantity)
  }
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)