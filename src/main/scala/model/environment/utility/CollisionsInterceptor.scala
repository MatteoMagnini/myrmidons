package model.environment.utility

import akka.actor.ActorRef
import common.geometry.Vector2D
import common.message.EnvironmentMessage.{FoodNear, NewPosition}
import model.environment.data.EnvironmentInfo
import model.environment.elements.EnvironmentElements.checkPositionIsInsideMoreObstacle
import model.environment.elements.{Food, Obstacle}

private[environment] object CollisionsInterceptor {

  /** Engine to find possible position inconsistencies.
   *
   * @param entity   reference to the insect to check
   * @param state    state of environment
   * @param position actual insect position
   * @param delta    insect shift (direction and magnitude)
   *
   * */
  def checkCollisions(entity: ActorRef, state: EnvironmentInfo, position: Vector2D, delta: Vector2D): Unit = {
    val newPosition = position >> delta
    import model.environment.elements.EnvironmentElements.FoodHasInside
    if (checkPositionIsInsideMoreObstacle(state.foods, position).isEmpty) {
      import model.environment.elements.EnvironmentElements.ObstacleHasInside
      val obstacleAndFood = state.obstacles ++ state.foods
      val obstacle = checkPositionIsInsideMoreObstacle(obstacleAndFood, newPosition)
      if (obstacle.nonEmpty) {
        val bouncedPosition = recursionCheck(obstacleAndFood, obstacle, position, newPosition)
        if (bouncedPosition._3) {
          val nearestFood: Food = state.foods.toList.sortWith((a, b) =>
            position --> a.position < position --> b.position).head
          entity ! FoodNear(nearestFood.position)
        }
        entity ! NewPosition(bouncedPosition._1, bouncedPosition._2)
      } else {
        entity ! NewPosition(newPosition, newPosition - position)
      }
    } else {
      entity ! NewPosition(position, delta)
    }
  }

  @scala.annotation.tailrec
  private def recursionCheck(obstacles: Iterable[Obstacle], obstacle: Iterable[Obstacle], position: Vector2D,
                             newPosition: Vector2D): (Vector2D, Vector2D, Boolean) = {
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    val res = handleObstacleIntersection(obstacle, position, newPosition)
    val intersection = res._1
    val delta = res._2
    val bouncedPos = intersection >> delta
    val newObstacles = checkPositionIsInsideMoreObstacle(obstacles, bouncedPos)
    if (newObstacles.isEmpty) {
      (bouncedPos, delta, res._3.get.isInstanceOf[Food])
    } else {
      recursionCheck(obstacles, newObstacles, intersection, bouncedPos)
    }
  }

  private def handleObstacleIntersection(obstacle: Iterable[Obstacle], position: Vector2D,
                                         newPosition: Vector2D): (Vector2D, Vector2D, Option[Obstacle]) = {

    val intersectionOnOverlappedObstacle = (for (f <- obstacle)
      yield (f.findIntersectionInformation(position, newPosition), f)).toList

    val intersectionAndDirectionOpt = intersectionOnOverlappedObstacle
      .filter(_._1.nonEmpty)
      .sortWith((a, b) => position --> a._1.intersectionPoint < position --> b._1.intersectionPoint).headOption

    if (intersectionAndDirectionOpt.nonEmpty) {
      val intersectionAndDirection = intersectionAndDirectionOpt._1.get
      val angleTest = if (intersectionAndDirection.angle < math.Pi / 2) {
        math.Pi - (intersectionAndDirection.angle * 2)
      } else {
        -((2 * intersectionAndDirection.angle) - math.Pi)
      }
      val newDelta = intersectionAndDirection.intersectionPoint - newPosition
      val orientedDelta = (
        (math.cos(angleTest) * newDelta.x) - (math.sin(angleTest) * newDelta.y),
        (math.sin(angleTest) * newDelta.x) + (math.cos(angleTest) * newDelta.y)
      )
      (intersectionAndDirection.intersectionPoint, orientedDelta, Some(intersectionAndDirectionOpt._2))
    } else {
      (position, (position - newPosition) / ((position - newPosition) ||), None)
    }
  }
}
