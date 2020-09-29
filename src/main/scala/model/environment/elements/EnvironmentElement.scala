package model.environment.elements

import model.environment.Boundary
import utility.geometry.{Vector2D, Vector3D}

  /** A strategy to define whether a position in inside an environment element
    *
    * @tparam A type of element
    */
  trait EnvironmentElement[A] {
    def hasInside(element: A, position: Vector2D): Boolean
  }

object EnvironmentElements {

  /**Returns, if exists, element having inside position
    *
    * @param elements collection of elements to check
    * @param position position against which compare elements
    * @tparam T context bound
    * @return if exists, element that have position inside of it
    */
  def checkHaveInside[T: EnvironmentElement](elements: Iterable[T], position: Vector2D): Option[T] =
    elements.find(x => implicitly[EnvironmentElement[T]].hasInside(x, position))

  /**Returns whether element has inside a position
    *
    * @param element element to check
    * @param position position against which compare element
    * @tparam T context bound
    * @return if element has position inside of it
    */
  def checkHasInside[T: EnvironmentElement](element: T, position: Vector2D): Boolean =
    implicitly[EnvironmentElement[T]].hasInside(element, position)

  /**How to check positions in [[model.environment.Boundary]] */
  implicit object BoundaryHasInside extends EnvironmentElement[Boundary] {
    override def hasInside(element: Boundary, position: Vector2D): Boolean =
      (position.x >= element.topLeft.x) && (position.x <= element.topRight.x) &&
        (position.y >= element.topLeft.y) && (position.y <= element.bottomLeft.y)
  }

  /**How to check positions in [[model.environment.elements.Food]] */
  implicit object FoodHasInside extends EnvironmentElement[Food] {
    override def hasInside(element: Food, position: Vector2D): Boolean =
      position --> element.position <= element.radius
  }

  /**How to check positions in [[model.environment.elements.Obstacle]] */
  implicit object ObstacleHasInside extends EnvironmentElement[Obstacle] {
    override def hasInside(element: Obstacle, coordinate: Vector2D): Boolean = {
      import utility.geometry.VectorsImplicits._

      val maxX = element.points.sortWith((a, b) => a.x > b.x) head
      //track an ray in right version
      val ray = coordinate X Vector3D(maxX.x + 1, coordinate.y, 1)
      var counter = 0
      //find intersection between polygon segment and ray
      val segments = element.segments
      segments.indices foreach (i => {
        val crossIntersection = segments(i)._3 X ray
        // intersection at ideal point (parallel line)
        if (crossIntersection.z != 0.0) {
          val intersection = crossIntersection / crossIntersection.z
          if ((((segments(i)._1.x >= intersection.x) && (segments(i)._2.x <= intersection.x))
            || ((segments(i)._1.x <= intersection.x) && (segments(i)._2.x >= intersection.x)))
            && intersection.x >= coordinate.x) {
            counter += 1
          }
        }
      })
      (counter % 2) != 0
    }
  }

}
