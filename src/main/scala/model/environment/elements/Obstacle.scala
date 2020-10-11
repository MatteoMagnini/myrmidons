package model.environment.elements

import model.Drawable
import utility.geometry.RandomVector2DInCircle
import utility.geometry.VectorsImplicits._
import utility.geometry.{Vector2D, Vector3D, Vectors}


/** An implementation of an obstacle.
 * It can accept every polygonal obstacle form.
 *
 * @param points list of vertex of polygon that describe an obstacle
 **/
class Obstacle(val points: List[Vector2D]) extends Drawable {

  override val position: Vector2D = Vectors.findCentroid(points)


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


  def maxDistanceFromCenter(): Double ={
    points.sortWith((p1, p2) => (p1 --> position) < (p2 --> position)).head --> position
  }

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

  /**
   * Given obstacle b, eliminate overlapped vertex and join other vertex in a single obstacle
   *
   * @param b obstacle to join.
   *
   * @return An Option of obstacle that is defined if join is well done, otherwise return None
   * */
  def ><(b:Obstacle): Option[Obstacle] = {
    if(this.isInstanceOf[Food] != b.isInstanceOf[Food])
      throw new IllegalArgumentException(s"$this and $b are different objects")

    if(this equals b)
      return Some(this)

    val newPointList = (this ->| b).toList
    if(newPointList.nonEmpty){
      val centroid = Vectors.findCentroid(newPointList)
      val ordered  = newPointList.sortWith((a,b) =>  ((a - centroid) /\) < ((b - centroid) /\))
      Some(Obstacle(ordered))
    } else None
  }

  /**
   * Check the overlap between this obstacle and b
   *
   * @param b obstacle to check overlap
   *
   * @return if overlapping are found, return a list of no overlapped vertex. Otherwise return an empty list
   * */
  def ->|(b:Obstacle): Iterable[Vector2D] = {
    import model.environment.elements.EnvironmentElements._
    val freePointOfA = points.filter(p => !checkHasInside(b, p))
    val freePointOfB = b.points.filter(p => !checkHasInside(this, p))

    val overlappedPoint = points.filter(p => b.points.exists(p2 => p.~~(p2,1E-4)))

    if((freePointOfA.size < points.size)
      || (freePointOfB.size < b.points.size)
      || overlappedPoint.nonEmpty
      || (position --> b.position < math.min(maxDistanceFromCenter(), b.maxDistanceFromCenter()))) {
      freePointOfA.diff(overlappedPoint)++ freePointOfB
    } else {
      overlappedPoint.foreach(x => println(x))
      List.empty
    }
  }


  def canEqual(other: Any): Boolean = other.isInstanceOf[Obstacle]

  override def equals(other: Any): Boolean = other match {
    case that: Obstacle =>
      ((that canEqual this)
        && position ~~(that.position, 1E-7)
        && this.points.size == that.points.size
        )
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(position, points.head)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
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
    }
      yield Vector2D(math.cos(angle * a) * radius, math.sin(angle * a) * radius) >> position

    Obstacle(vertex.toList)
  }

  /**
   * Factory for regular triangle
   * */
  def Triangle(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 3)

  /**
   * Factory for Square
   * */
  def Square(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 4)

  /**
   * Factory for Octagon
   * */
  def Octagon(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 8)

  /**
   * Create random random obstacles.
   *
   * @param nObstacle number of obstacle to join
   * @param center center of spawn
   * @param minMaxDistanceFromCenter (min,Max) distance from center that delimit area to spawn object
   *
   * @return list of spawned obstacle, this list should have size < of nObstacle because someone could be joined
   * */
  def createRandom(nObstacle:Int, center: Vector2D, minMaxDistanceFromCenter: (Double, Double), radius: Double = 20):Iterable[Obstacle] = {

    val obstacles = for {i <- 0 until nObstacle
      random = scala.util.Random.nextInt(10) + 3
      obstacle = Obstacle(
        RandomVector2DInCircle(minMaxDistanceFromCenter, center),
        radius,
        random
      )
    } yield obstacle

    recursiveJoin(obstacles.toList, center, 0)
  }

  @scala.annotation.tailrec
  private def recursiveJoin(obstacles: List[Obstacle], center: Vector2D, index: Int):Iterable[Obstacle]= {
    if (index >= obstacles.size)
      return obstacles

    val nextIndex = if (index == obstacles.size - 1) 0 else index + 1
    val orderedObstacle = obstacles.sortWith((a, b) => ((a.position - center) /\) < ((b.position - center) /\))
    val joinedObstacle = orderedObstacle(index) >< orderedObstacle(nextIndex)

    if (joinedObstacle.isDefined) {
      val newObstacleList = joinedObstacle.head +: (orderedObstacle diff List(orderedObstacle(index), orderedObstacle(nextIndex)))
      recursiveJoin(newObstacleList, center, index)
    } else recursiveJoin(orderedObstacle, center, index + 1)
  }
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)
