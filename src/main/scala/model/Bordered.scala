package model

import scala.language.implicitConversions
import utility.Geometry.{RandomVector2DInSquare, Vector2D}

/**
 * trait for a entity that present a border
 **/
trait Bordered extends Drawable {
  /**
   * function to verify if an entity has inside itself an
   * position.
   *
   * @param coordinate to check
   * @return true if coordinate is inside of the obstacle
   **/
  def hasInside(coordinate: Vector2D): Boolean

  /**
<<<<<<< HEAD
    * Find the intersection point of a segment defined by two point
    * (oldPosition, newPosition) with an obstacle border.
    *
    * @param oldPosition of the object
    * @param newPosition of the object (must be inside the obstacle)
    *
    * @throws IllegalArgumentException if newPosition is outside the
    *                                  obstacle
    *
    * @return an instance of IntersectionResult case class with the
    *         position of intersection and the smallest angle formed
    *         between obstacle border line and object trajectory.
    *         If return a zero position and an angle with value of
    *         Double.MaxValue, then there are no valid intersection
    *         or something wrong happened
    * */
  def findIntersectionPoint(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult]
}

object BorderedEntityFactory {
  /**
   * Create and instance of simple obstacle in a random position
   * with dimension xDim on x-axis and yDim on y-axis.
   *
   * @param minPos lowest value for position (both x and y coordinate)
   * @param maxPos highest value for position (both x and y coordinate)
   * @param xDim   dimension on x-axis
   * @param yDim   dimension on y-axis
   * @return a SimpleObstacle instance
   */
  def createRandomSimpleObstacle(minPos: Double = 0, maxPos: Double = 800, xDim: Int = 30, yDim: Int = 30):SimpleObstacle = {
    var pos = RandomVector2DInSquare(minPos, maxPos)
    if (pos --> Vector2D(400,400) < 50)
      pos = pos >> Vector2D(if(pos.x < 400) -50 else 50,0)

    new SimpleObstacle(Vector2D(pos.x, pos.y), xDim, yDim)
  }


  /**
   * Create and instance of food obstacle in a random position
   * and given quantity of food
   *
   * @param minPos   lowest value for position (both x and y coordinate)
   * @param maxPos   highest value for position (both x and y coordinate)
   * @param quantity of food of this resource
   *
   **/
  def createRandomFood(minPos: Double = 0, maxPos: Double = 800, quantity: Int = 500): Food = {
    val pos = RandomVector2DInSquare(minPos, maxPos)
    Food(Vector2D(pos.x, pos.y), quantity)
  }
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)