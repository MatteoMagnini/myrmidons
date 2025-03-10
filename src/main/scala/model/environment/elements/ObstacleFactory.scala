package model.environment.elements

import common.geometry.Vector2D
import common.geometry.Vector2DFactory.{RandomVector2DInCircle, ZeroVector2D}
import model.environment.elements.EnvironmentElements.checkPositionIsInsideMoreObstacle

import scala.util._

/**
 * Obstacle factory.
 **/
object ObstacleFactory {

  import model.environment._

  /** Obstacle by vertices list. */
  def apply(points: List[Vector2D]): Obstacle = new Obstacle(points)

  /** Obstacle by position and number of sides  */
  def apply(position: Vector2D, radius: Double = OBSTACLE_RADIUS, nSides: Int): Obstacle = {

    val angle = 2 * math.Pi / nSides
    val vertex = for {
      a <- 0 until nSides
    } yield Vector2D(math.cos(angle * a) * radius, math.sin(angle * a) * radius) >> position

    ObstacleFactory(vertex.toList)
  }

  /** Factory for regular triangle */
  def Triangle(position: Vector2D, radius: Double = OBSTACLE_RADIUS): Obstacle = {
    ObstacleFactory(position, radius, OBSTACLE_TRIANGLE_VERTEX)
  }

  /** Factory for Square */
  def Square(position: Vector2D, radius: Double = OBSTACLE_RADIUS): Obstacle = {
    ObstacleFactory(position, radius, OBSTACLE_SQUARE_VERTEX)
  }

  /**
   * Create random random obstacles.
   *
   * @param nObstacle number of obstacle to join
   * @param center    center of spawn
   * @return list of spawned obstacle, this list should have size < of nObstacle because someone could be joined
   **/
  def createRandom(nObstacle: Int, center: Vector2D, minDistance: Double, maxDistance: Double,
                   radius: Double = OBSTACLE_RADIUS, obstacleVertex: Int = OBSTACLE_DEFAULT_VERTEX)
  : Iterable[Obstacle] = {
    val obstacles = for {_ <- 0 until nObstacle
                         random = Random.nextInt(obstacleVertex) + OBSTACLE_TRIANGLE_VERTEX
                         obstacle = ObstacleFactory(RandomVector2DInCircle(minDistance, maxDistance, center),
                           radius, random)
                         } yield obstacle

    recursiveJoin(obstacles.toList, center, 0)
  }

  @scala.annotation.tailrec
  private def recursiveJoin(obstacles: List[Obstacle], center: Vector2D, index: Int): Iterable[Obstacle] = {
    if (index >= obstacles.size - 1) {
      obstacles
    } else {
      val nextIndex = if (index == obstacles.size - 1) 0 else index + 1
      val orderedObstacle = obstacles.sortWith((a, b) => ((a.position - center) /\) < ((b.position - center) /\))
      val joinedObstacle = orderedObstacle(index) >< orderedObstacle(nextIndex)

      if (joinedObstacle.isDefined) {
        val obsDiffJoined = orderedObstacle diff List(orderedObstacle(index), orderedObstacle(nextIndex))
        val newObstacleList = joinedObstacle.head +: obsDiffJoined
        recursiveJoin(newObstacleList, center, index)
      } else {
        recursiveJoin(orderedObstacle, center, index + 1)
      }
    }
  }

  /** Random position out of present obstacles.
   *
   * @param obstacleList obstacles
   * @param min          min radius
   * @param max          max radius
   * @param center       center of spawn area
   * @return position
   */
  def randomPositionOutObstacleFromCenter(obstacleList: Seq[Obstacle], center: Vector2D,
                                          min: Double, max: Double): Vector2D = {
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    var randomPosition = ZeroVector2D()
    do {
      randomPosition = RandomVector2DInCircle(min, max, center)
    }
    while (checkPositionIsInsideMoreObstacle(obstacleList, randomPosition).nonEmpty)
    randomPosition
  }
}
