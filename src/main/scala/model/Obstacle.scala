package  model

import utility.Geometry.{Vector2D, Vector3D}

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
  override def hasInside(coordinate: Vector2D): Boolean = {
    if((coordinate.x > (position.x - xDim/2))
      && (coordinate.x < (position.x + xDim/2))
      && (coordinate.y > (position.y - yDim/2))
      && (coordinate.y < (position.y + yDim/2))) {
      true
    }
    else false
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
}
