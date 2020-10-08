package model.environment

import akka.actor.ActorRef
import model.environment.elements.EnvironmentElements.{checkHasInside, checkHaveInside}
import model.environment.elements.{Food, Obstacle}
import utility.Messages.{FoodNear, NewPosition}
import utility.geometry.Vector2D

object CollisionsInterceptor {

  def checkCollisions(environmentRef: ActorRef, entity: ActorRef, state: EnvironmentInfo, position: Vector2D, delta: Vector2D): Unit = {
    val newPosition = position >> delta
    /*Checking boundary*/
    import model.environment.elements.EnvironmentElements.BoundaryHasInside
    if (checkHasInside(state.boundary, newPosition)) {
      /*Checking obstacles*/
      import model.environment.elements.EnvironmentElements.ObstacleHasInside
      val obstacle = checkHaveInside(state.obstacles, newPosition)
      if (obstacle.nonEmpty) {
        val bouncedPosition = recursionCheck(state.obstacles, obstacle, position, newPosition)
        entity ! NewPosition(bouncedPosition._1, bouncedPosition._2)

      } else {
        /*Checking food sources*/
        import model.environment.elements.EnvironmentElements.FoodHasInside
        val food = checkHaveInside(state.foods, newPosition)
        if (food.nonEmpty) {
          val bouncingResult = recursionCheck(state.foods, food, position, newPosition)
          val nearestFood: Food = food.toList.sortWith((a, b) =>  //TODO: Not correct in all case
            position --> a.position < position --> b.position).head
          entity ! FoodNear(nearestFood.position)
          entity ! NewPosition(bouncingResult._1 , bouncingResult._2) // TODO: should bounce also on food!

        } else {
          entity ! NewPosition(newPosition, newPosition - position)
        }
      }
    } else entity ! NewPosition(position - delta, delta -)
  }

  @scala.annotation.tailrec
  private def recursionCheck(obstacles:Iterable[Obstacle], obstacle: Iterable[Obstacle], position: Vector2D, newPosition: Vector2D):(Vector2D, Vector2D) = {
    val res = handleObstacleIntersection(obstacle, position, newPosition)
    val intersection = res._1
    val delta = res._2

    val bouncedPos = intersection >> delta
    import model.environment.elements.EnvironmentElements.ObstacleHasInside
    val o = checkHaveInside(obstacles, bouncedPos)
    if (o.isEmpty) {
      (bouncedPos, delta)
    } else {
      recursionCheck(obstacles, o, intersection, bouncedPos)
    }
  }

  private def handleObstacleIntersection(obstacle: Iterable[Obstacle], position: Vector2D, newPosition: Vector2D): (Vector2D, Vector2D) = {
    import Implicits._

    val intersectionOnOverlappedObstacle = (for (f <- obstacle)
      yield f.findIntersectionInformation(position, newPosition)).toList

    val intersectionAndDirectionOpt = intersectionOnOverlappedObstacle
      .filter(_.nonEmpty)
      .sortWith((a, b) => position --> a.intersectionPoint < position --> b.intersectionPoint).headOption

    if (intersectionAndDirectionOpt.nonEmpty) {
      val intersectionAndDirection = intersectionAndDirectionOpt.get
      val angleTest = if (intersectionAndDirection.angle < math.Pi / 2)
        math.Pi - (intersectionAndDirection.angle * 2)
      else
        -((2 * intersectionAndDirection.angle) - math.Pi)
      val newDelta = intersectionAndDirection.intersectionPoint - newPosition
      val orientedDelta = (
        (math.cos(angleTest) * newDelta.x) - (math.sin(angleTest) * newDelta.y),
        (math.sin(angleTest) * newDelta.x) + (math.cos(angleTest) * newDelta.y)
      )
      import utility.geometry.TupleOp2._
      (intersectionAndDirection.intersectionPoint, orientedDelta)
    } else {
      (position, (position - newPosition)/(((position - newPosition)|| )))
    }

  }

}
