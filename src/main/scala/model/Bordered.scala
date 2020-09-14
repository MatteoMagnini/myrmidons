package model

import scala.language.implicitConversions
import utility.Geometry.{RandomVector2D, Vector2D}
import utility.Geometry.TupleOp._

/**
 * trait for a entity that present a border
 * */
trait Bordered{
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
   * Create an obstacle in a random position in coordinate
   * between minPos and maxPos, with dimension xDim on x-axis
   * and yDim on y-axis.
   *
   * @param minPos lowest value for position (both x and y coordinate)
   * @param maxPos highest value for position (both x and y coordinate)
   * @param xDim dimension on x-axis
   * @param yDim dimension on y-axis
   *
   * @return an EnvObstacle in a random position
   * */
 /* def createRandomComplexObstacle(minPos: Double = 50, maxPos: Double = 550, xDim: Double = 100, yDim: Double = 50): Obstacle = {
    val pos = RandomVector2D(minPos, maxPos)
    Obstacle(List(
      (pos.x - xDim/2, pos.y - yDim/2, 1.0),
      (pos.x + xDim/2, pos.y - yDim/2, 1.0),
      (pos.x + xDim/2, pos.y + yDim/2, 1.0),
      (pos.x - xDim/2, pos.y + yDim/2, 1.0)
    ))
  }
*/
  /**
   * Create and instance of simple obstacle in a random position
   * with dimension xDim on x-axis and yDim on y-axis.
   *
   * @param minPos lowest value for position (both x and y coordinate)
   * @param maxPos highest value for position (both x and y coordinate)
   * @param xDim dimension on x-axis
   * @param yDim dimension on y-axis
   *
   * @return a SimpleObstacle istance
   * */
  def createRandomSimpleObstacle(minPos: Double = 50, maxPos: Double = 550, xDim: Int = 100, yDim: Int = 50):SimpleObstacle = {
    val pos= RandomVector2D(minPos, maxPos)
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
  def createRandomFood(minPos: Double = 50, maxPos: Double = 550, quantity: Int = 100):Food = {
    val pos= RandomVector2D(minPos, maxPos)
    Food(Vector2D(pos.x, pos.y), quantity)
  }
}