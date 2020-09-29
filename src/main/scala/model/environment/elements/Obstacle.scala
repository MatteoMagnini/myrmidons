package model.environment.elements

import model.Drawable
import utility.Geometry.{Vector2D, Vector3D}

/**An implementation of an obstacle.
 * It can accept every polygonal obstacle form.
 *
 * @param points list of vertex of polygon that describe an obstacle
 * */
case class Obstacle(points: List[Vector3D]) extends Drawable {

  override val position: Vector2D = findCentroid(points)
  var segments: List[(Vector3D, Vector3D, Vector3D)] = List()

  if (points.size < 3) {
    throw new IllegalArgumentException("points list must have more than 2 elements")
  }
  //get a line given two points
  points.indices foreach (i => {
    val before = if (i == 0) points.length - 1 else i - 1
    val product = points(before) X points(i)
    val line = product / product.z
    segments ::= (points(before), points(i), line)
  })

  /**
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
  def findIntersectionPoint(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult] = ???

  // a segments is described as a two point and a line pass through them
  private def findCentroid(l: List[Vector3D]): Vector2D = {
    import utility.Geometry.TupleOp._
    val centroid = points.foldRight(Vector3D(0.0, 0.0, 0.0))(_ >> _) / points.size
    val normalizedCentroid = centroid / centroid.z
    (normalizedCentroid.x, normalizedCentroid.y)
  }
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)
