package  model

import utility.Geometry.{Vector2D, Vector3D, ZeroVector2D}
import utility.Geometry.TupleOp3._
import utility.Geometry.TupleOp._

/**
 * SimpleObstacle represent a boxed obstacle.
 *
 * The coordinate of SimpleObstacle are defined as:
 *  - Bottom-Left: (position.x - xDim / 2, position.y - yDim / 2)
 *  - Bottom-Right: (position.x + xDim / 2, position.y - yDim / 2)
 *  - Top-Right: (position.x + xDim / 2, position.y + yDim / 2)
 *  - Top-Left: (position.x - xDim / 2, position.y + yDim / 2)
 *
 * hasInside implementation is easiest than Obstacle class.
 * */
class SimpleObstacle(override val position: Vector2D, val xDim: Double, val yDim: Double) extends Bordered {

  /**
   * function to verify if an entity has inside itself an
   * position.
   *
   * @param coordinate to check
   * @return true if coordinate is inside of the obstacle
   **/
  override def hasInside(coordinate: Vector2D): Boolean =
    ((coordinate.x > (position.x - xDim / 2))
      && (coordinate.x < (position.x + xDim / 2))
      && (coordinate.y > (position.y - yDim / 2))
      && (coordinate.y < (position.y + yDim / 2)))

  /**
   * Find the intersection point of a segment defined by two point
   * (oldPosition, newPosition) with an obstacle border.
   *
   * @param oldPosition of the object
   * @param newPosition of the object (must be inside the obstacle)
   *
   *
   * @return an instance of IntersectionResult case class with the
   *         position of intersection and the smallest angle formed
   *         between obstacle border line and object trajectory.
   *         If any border intersection are found it's return an Option.empty
   * */
  override def findIntersectionPoint(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult] = {

    //form vertex definition
    val vertex: List[Vector2D] = List(
      (position.x - xDim / 2, position.y - yDim / 2),
      (position.x + xDim / 2, position.y - yDim / 2),
      (position.x + xDim / 2, position.y + yDim / 2),
      (position.x - xDim / 2, position.y + yDim / 2)
    )

    //(start, stop, line)
    var segments: List[(Vector3D, Vector3D, Vector3D)] = List()
    
    //border segment definition
    vertex.indices foreach( i => {
      val before = if (i == 0) vertex.length - 1 else i - 1
      val line = vertex(before) X vertex(i)
      segments ::= (vertex(before), vertex(i), line)
    })

    // ant path definition
    val antPath: (Vector3D,Vector3D,Vector3D) = (oldPosition, newPosition, oldPosition X newPosition)

    var intersections: List[IntersectionResult] = List()
    /*
    * Calculate intersection between insect line and obstacle border line,
    * then check if intersection fall inside ant segment. If this check result
    * true, then the calculated intersection is the result with angle between
    * form border and ant path.
    * */
    segments.indices foreach( i => {
      val crossIntersection = segments(i)._3 X antPath._3
      if (crossIntersection.z != 0.0) {
        val intersection = crossIntersection / crossIntersection.z
        if ((intersection checkInside(segments(i)._1, segments(i)._2))
          && (intersection checkInside(oldPosition, newPosition))) {
          intersections = IntersectionResult((intersection.x, intersection.y), antPath._3 ^ segments(i)._3) +: intersections
        }
      }
    })
    if (intersections.size <= 0)
      Option.empty
    else
      Some(intersections.sortWith((a,b) => a.intersectionPoint-->oldPosition < b.intersectionPoint --> oldPosition).head)
  }

  def unapply(arg: SimpleObstacle): Option[(Vector2D, Double, Double)] = {
    Some(position, xDim, yDim)
  }
}

/**
 * An implementation of bordered obstacle.
 * This class can accept every polygonal obstacle form. The algorithm
 * to elaborate hasInside is more complex respect SimpleObstacle class
 *
 * @param points list of vertex of polygon that describe an obstacle
 * */
case class Obstacle(points: List[Vector3D]) extends Bordered {

  override val position: Vector2D = findCentroid(points)
  var segments: List[(Vector3D, Vector3D, Vector3D)] = List()

  if (points.size < 3) {
    throw new IllegalArgumentException("points list must have more than 2 elements")
  }
  //get a line given two points
  points.indices foreach (i => {
    var before = if (i == 0) points.length - 1 else i - 1
    val product = points(before) X points(i)
    val line = product / product.z
    segments ::= (points(before), points(i), line)
  })

  override def hasInside(coordinate: Vector2D): Boolean = {
    import utility.Geometry.TupleOp3._
    var maxX = points.sortWith((a, b) => a.x > b.x) head
    //track an ray in right version
    val ray = coordinate X Vector3D(maxX.x + 1, coordinate.y, 1)
    var counter = 0
    //find intersection between polygon segment and ray
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
  override def findIntersectionPoint(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult] = ???

  // a segments is described as a two point and a line pass through them
  private def findCentroid(l: List[Vector3D]): Vector2D = {
    import utility.Geometry.TupleOp._
    val centroid = points.foldRight(Vector3D(0.0, 0.0, 0.0))(_ >> _) / points.size
    val normalizedCentroid = centroid / centroid.z
    (normalizedCentroid.x, normalizedCentroid.y)
  }
}
