package model.environment.elements

import model.Drawable
import model.environment.elements
import utility.geometry.{Vector2D, Vector3D, Vectors}
import utility.geometry.VectorsImplicits._

/** An implementation of an obstacle.
 * It can accept every polygonal obstacle form.
 *
 * @param points list of vertex of polygon that describe an obstacle
 **/
case class Obstacle(points: List[Vector2D]) extends Drawable {

  override val position: Vector2D = findCentroid(points)


  var segments: List[(Vector2D, Vector2D, Vector3D)] = List()

  if (points.size < 3) {
    print(points.size)
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
   * @throws IllegalArgumentException if newPosition is outside the
   *                                  obstacle
   * @return an instance of IntersectionResult case class with the
   *         position of intersection and the smallest angle formed
   *         between obstacle border line and object trajectory.
   *         If return a zero position and an angle with value of
   *         Double.MaxValue, then there are no valid intersection
   *         or something wrong happened
   **/
  def findIntersectionInformation(oldPosition: Vector2D, newPosition: Vector2D): Option[IntersectionResult] = {
    println(s"Obstacle: $position")
    println(s"AntPos: $oldPosition")
    println(s"newPosition: $newPosition")
    //segments foreach( _ => println(_))
    // ant path definition
    val antPath: (Vector2D, Vector2D, Vector3D) = (oldPosition, newPosition, oldPosition X newPosition)
    var intersections: List[IntersectionResult] = List()

    /*
    * Calculate intersection between insect line and obstacle border line,
    * then check if intersection fall inside ant segment. If this check result
    * true, then the calculated intersection is the result with angle between
    * form border and ant path.
    * */
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
    else Some(intersections.sortWith(
      (a, b) =>
        (a.intersectionPoint --> oldPosition) < (b.intersectionPoint --> oldPosition)
    ).head
    )
  }

  // a segments is described as a two point and a line pass through them
  private def findCentroid(l: List[Vector2D]): Vector2D = {
    l.foldRight(Vector2D(0.0, 0.0))(_ >> _) / l.size
  }


}

/**
 * Obstacle factory.
 **/
object Obstacle {
  /**
   * Obstacle by vertex list.
   **/
  def apply(points: List[Vector2D]): Obstacle = new Obstacle(points)

  /**
   * Obstacle by position and number of sides
   **/
  def apply(position: Vector2D, radius: Double = 10, nSides: Int): Obstacle = {


    val angle = 2 * math.Pi / nSides

    val vertex = for {
      a <- 0 until nSides
      //test <- angle * a
    }
      yield (Vector2D(math.cos(angle * a) * radius, math.sin(angle * a) * radius) >> position)

    Obstacle(vertex.toList)
  }

  def Triangle(position: Vector2D, radius: Double = 10) = Obstacle(position, radius, 3)

  def Square(position: Vector2D, radius: Double = 10) = Obstacle(position, radius, 4)

  def Octagon(position: Vector2D, radius: Double = 10) = Obstacle(position, radius, 8)
  val listValue = List(3, 4, 8) // TODO refactor magic number
  def randomValid: Int = listValue(scala.util.Random.nextInt(listValue.size))
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)
