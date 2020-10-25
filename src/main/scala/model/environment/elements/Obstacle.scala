
package model.environment.elements

import common.geometry.{Vector2D, Vector3D, Vectors}
import model.Drawable

/** An implementation of an obstacle.
 * It can accept every polygonal obstacle form.
 *
 * @param points list of vertex of polygon that describe an obstacle
 **/
class Obstacle(val points: List[Vector2D]) extends Drawable {

  override val position: Vector2D = Vectors.findCentroid(points)
  var segments: List[(Vector2D, Vector2D, Vector3D)] = List()

  if (points.size < 3) {
    throw new IllegalArgumentException("points list must have more than 2 elements")
  }

  points.indices foreach (i => {
    val before = if (i == 0) points.length - 1 else i - 1
    val product = points(before) X points(i)
    val line = product / product.z
    segments ::= (points(before), points(i), line)
  })


  /** Find the intersection point of a segment with an obstacle border.
   *
   * @param oldPosition of the object
   * @param newPosition of the object (must be inside the obstacle)
   * @return an instance of IntersectionResult case class with the
   *         position of intersection and his angle.
   **/
  def findIntersectionInformation(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult] = {

    val antPath: (Vector2D, Vector2D, Vector3D) = (oldPosition, newPosition, oldPosition X newPosition)
    var intersections: List[IntersectionResult] = List()

    segments.indices foreach (i => {
      val intersectionPoint = Vectors.findIntersectionPoint(segments(i), antPath)
      if (intersectionPoint != Option.empty) {
        val angle = Vectors.findIntersectionAngle(segments(i), antPath)
        intersections = IntersectionResult(intersectionPoint.get, angle) +: intersections
      }
    })

    if (intersections.size <= 0) {
      Option.empty
    }
    else {
      Some(intersections.sortWith((a, b) =>
        (a.intersectionPoint --> oldPosition) < (b.intersectionPoint --> oldPosition)).head
      )
    }
  }

  /**
   * Given obstacle, eliminate overlapped vertices and join other vertices in a single obstacle
   *
   * @param obstacle obstacle to join.
   * @return An Option of obstacle that is defined if join is well done, otherwise return None
   **/
  def ><(obstacle: Obstacle): Option[Obstacle] = {
    if (this.isInstanceOf[Food] != obstacle.isInstanceOf[Food]) {
      throw new IllegalArgumentException(s"$this and $obstacle are different objects")
    }

    if (this equals obstacle) {
      Some(this)
    } else {
      val newPointList = (this ->| obstacle).toList
      if (newPointList.nonEmpty) {
        val centroid = Vectors.findCentroid(newPointList)
        val ordered = newPointList.sortWith((a, b) => ((a - centroid) /\) < ((b - centroid) /\))
        Some(ObstacleFactory(ordered))
      } else {
        None
      }
    }
  }

  /** Check the overlap between this and other obstacle.
   *
   * @param obstacle obstacle to check overlap
   * @return if overlaps, return a list of no overlapped vertices. Otherwise return an empty list
   **/
  def ->|(obstacle: Obstacle): Iterable[Vector2D] = {
    import model.environment.elements.EnvironmentElements._
    val freePointOfA = points.filter(p => !checkPositionIsInsideObstacle(obstacle, p))
    val freePointOfB = obstacle.points.filter(p => !checkPositionIsInsideObstacle(this, p))
    val overlappedPoint = points.filter(p => obstacle.points.exists(p2 => p.~~(p2, 1E-4)))

    if ((freePointOfA.size < points.size)
      || (freePointOfB.size < obstacle.points.size)
      || overlappedPoint.nonEmpty
      || (position --> obstacle.position < math.min(maxDistanceFromCenter(), obstacle.maxDistanceFromCenter()))) {
      freePointOfA.diff(overlappedPoint) ++ freePointOfB
    } else {
      List.empty
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[Obstacle]

  override def equals(other: Any): Boolean = other match {
    case that: Obstacle =>
      ((that canEqual this)
        && position ~~ (that.position, 1E-7)
        && this.points.size == that.points.size
        )
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(position, points.head)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  private def maxDistanceFromCenter(): Double = {
    points.sortWith((point1, point2) => (point1 --> position) < (point2 --> position)).head --> position
  }
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)
