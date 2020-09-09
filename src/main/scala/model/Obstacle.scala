package model

import scala.language.implicitConversions

import utility.Geometry.Vector3D

trait Obstacle{
  /**
   * function to verify if a coordinate is inside an obstacle
   *
   * @param coordinate to check
   *
   * @return true if coordinate is inside of the obstacle
   * */
  def isInside(coordinate: Vector3D):Boolean
}

/**
 * An implementation of obstacle.
 *
 * @param points list of vertex of polygon that describe an obstacle
 * */
class EnvObstacle(val points: List[Vector3D]) extends Obstacle {
  // a segments is described as a two point and a line pass through them
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

  override def isInside(coordinate: Vector3D): Boolean = {
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
}
