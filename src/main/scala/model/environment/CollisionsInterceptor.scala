
package model.environment

import akka.actor.ActorRef
import model.environment.elements.EnvironmentElements.{checkHasInside, checkHaveInside}
import model.environment.elements.{Food, Obstacle}
import model.environment.info.EnvironmentInfo
import utility.Messages.{FoodNear, NewPosition}
import utility.geometry.Vector2D

private[environment] object CollisionsInterceptor {

  /**
   * Extension of Environment move function. His job is check
   * if the new position of selected by insect fall inside an
   * obstacle and in that case must recalculate new insect position
   * keep mind the bouncing on obstacle and obstacle form.
   *
   * @param entity ActorRef of insect that need the position
   *               check
   * @param state state of environment, with updated information
   *              of obstacle and/or food
   * @param position actual insect position
   * @param delta insect shift (direction and magnitude)
   *
   * */
  def checkCollisions(entity: ActorRef, state: EnvironmentInfo, position: Vector2D, delta: Vector2D): Unit = {
    val newPosition = position >> delta
    /*Checking boundary*/
    import model.environment.elements.EnvironmentElements.BoundaryHasInside
    if (checkHasInside(state.boundary, newPosition)) {
      /*Checking obstacles*/
      import model.environment.elements.EnvironmentElements.ObstacleHasInside
      val of = state.obstacles ++ state.foods
      val obstacle = checkHaveInside(of, newPosition)
      if (obstacle.nonEmpty) {
        val bouncedPosition = recursionCheck(of, obstacle, position, newPosition)
        if(bouncedPosition._3){
          val nearestFood: Food = state.foods.toList.sortWith((a, b) =>  //TODO: Not correct in all case
            position --> a.position < position --> b.position).head
          entity ! FoodNear(nearestFood.position)
        }
        entity ! NewPosition(bouncedPosition._1, bouncedPosition._2)
      } else {
        entity ! NewPosition(newPosition, newPosition - position)
      }
    } else {entity ! NewPosition(position - delta, delta -)}
  }

  /**
   * when a new insect position fall inside an obstacle, this
   * function calculate new bounced position and ricursively check
   * that it fall outside another obstacle. If fall inside other
   * obstacle, calculate a bounced position on second obstacle
   *
   * @param obstacles list of all obstacle present inside the map
   * @param obstacle list of obstacle where new insect position fall in
   * @param position of the insect
   * @param newPosition new shifted position (position>>delta)
   *
   * @return (position, delta, isFood)
   *         1: the new bounced position
   *         2: the new delta of insect
   *         3: a flag that define if the obstacle is a food or not
   * */
  @scala.annotation.tailrec
  private def recursionCheck(obstacles:Iterable[Obstacle],
                             obstacle: Iterable[Obstacle],
                             position: Vector2D,
                             newPosition: Vector2D)
  :(Vector2D, Vector2D, Boolean) = {
    val res = handleObstacleIntersection(obstacle, position, newPosition)
    val intersection = res._1
    val delta = res._2

    val bouncedPos = intersection >> delta
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    val o = checkHaveInside(obstacles, bouncedPos)
    if (o.isEmpty) {
      (bouncedPos, delta, res._3.get.isInstanceOf[Food])
    } else {
      recursionCheck(obstacles, o, intersection, bouncedPos)
    }
  }

  /**
   * This function calculates the intersection point of insect and the obstacles
   * passed by arguments.
   * It also calculate new delta orientation
   *
   * @param obstacle list of obstacle where new insect position fall in
   * @param position of the insect
   * @return (position, delta, isFood)
   *         1: the new bounced position
   *         2: the new delta of insect
   *         3: obstacle on which an insect is bounced
   * */
  private def handleObstacleIntersection(obstacle: Iterable[Obstacle],
                                         position: Vector2D,
                                         newPosition: Vector2D)
  : (Vector2D, Vector2D, Option[Obstacle]) = {

    val intersectionOnOverlappedObstacle = (for (f <- obstacle)
      yield (f.findIntersectionInformation(position, newPosition),f)).toList

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
      import utility.geometry.TupleOp2._
      (intersectionAndDirection.intersectionPoint, orientedDelta, Some(intersectionAndDirectionOpt._2))
    } else {
      (position, (position - newPosition)/((position - newPosition)||), None)
    }
  }
}
