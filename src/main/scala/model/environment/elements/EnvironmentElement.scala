package model.environment.elements

import model.environment.Boundary
import common.geometry.{Vector2D, Vector3D, Vectors}

/** A strategy to define whether a position in inside an environment element.
  *
  * @tparam A type of element
  */
trait EnvironmentElement[A] {
  def hasInside(element: A, position: Vector2D): Boolean
}

object EnvironmentElements {

  /** Returns, if exists, element having inside position.
    *
    * @param elements collection of elements to check
    * @param position position against which compare elements
    * @tparam T context bound
    * @return if exists, element that have position inside of it
    */
  def checkPositionIsInsideMoreObstacle[T: EnvironmentElement](elements: Iterable[T], position: Vector2D): Iterable[T] =
    elements.filter(x => implicitly[EnvironmentElement[T]].hasInside(x, position))

  /** Returns whether element has inside a position.
    *
    * @param element  element to check
    * @param position position against which compare element
    * @tparam T context bound
    * @return if element has position inside of it
    */
  def checkPositionIsInsideObstacle[T: EnvironmentElement](element: T, position: Vector2D): Boolean =
    implicitly[EnvironmentElement[T]].hasInside(element, position)

  /** How to check positions in [[model.environment.Boundary]] */
  implicit object BoundaryHasInside extends EnvironmentElement[Boundary] {
    override def hasInside(element: Boundary, position: Vector2D): Boolean =
      (position.x >= element.topLeft.x) && (position.x <= element.topRight.x) &&
        (position.y >= element.bottomLeft.y) && (position.y <= element.topLeft.y)
  }

  /** How to check positions in [[model.environment.elements.Food]] */
  implicit object FoodHasInside extends EnvironmentElement[Food] {
    override def hasInside(element: Food, position: Vector2D): Boolean =
      implicitly[EnvironmentElement[Obstacle]].hasInside(element, position)
  }

  /** How to check positions in [[model.environment.elements.Obstacle]] */
  implicit object ObstacleHasInside extends EnvironmentElement[Obstacle] {
    override def hasInside(element: Obstacle, coordinate: Vector2D): Boolean = {

      val maxX = element.points.sortWith((a, b) => a.x > b.x) head
      val stopCheckRay = Vector3D(maxX.x + 1, coordinate.y, 1)
      val ray: (Vector2D, Vector2D, Vector3D) = (coordinate, stopCheckRay, coordinate X stopCheckRay)
      var counter = 0
      val segments = element.segments
      segments.indices foreach (i => {
        val crossIntersection = Vectors.findIntersectionPoint(segments(i), ray)
        if (crossIntersection != Option.empty) {
          counter += 1
        }
      })
      (counter % 2) != 0
    }
  }

}
