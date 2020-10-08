package model.environment.elements

import model.Drawable
import model.environment.elements
import model.environment.elements.EnvironmentElements.{checkHasInside, checkHaveInside}
import utility.geometry.{RandomVector2DInCircle, Vector2D, Vector3D, Vectors, ZeroVector2D}
import utility.geometry.VectorsImplicits._

import scala.runtime.SymbolLiteral

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


  def maxDistanceFromCenter() ={
    points.sortWith((p1, p2) => (p1-->position) < (p2--> position)).head --> position
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
    //println(s"Obstacle: $position")
    //println(s"AntPos: $oldPosition")
    //println(s"newPosition: $newPosition")
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


  def canEqual(other: Any): Boolean = other.isInstanceOf[Obstacle]

  override def equals(other: Any): Boolean = other match {
    case that: Obstacle =>
      (that canEqual this) &&
        position ~~(that.position, 1E-6)
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(position)
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

  def Triangle(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 3)

  def Square(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 4)

  def Octagon(position: Vector2D, radius: Double = 10): Obstacle = Obstacle(position, radius, 8)

  def createRandom(nObstacle:Int, center: Vector2D, minMaxDistanceFromCenter: (Double, Double), radius: Double = 20):Iterable[Obstacle] = {

    val obstacles = (for {i <- 0 until nObstacle
      random = Obstacle.randomValid
      obstacle = Obstacle(
        RandomVector2DInCircle(minMaxDistanceFromCenter, center),
        radius,
        random
      )
    } yield obstacle)
    //obstacles
    recursiveJoin(obstacles.toList, center, 0)
  }

   @scala.annotation.tailrec
  def recursiveJoin(obstacles: List[Obstacle], center: Vector2D, index: Int):Iterable[Obstacle]= {
    if(index >= obstacles.size)
      return obstacles

    val nextIndex = if(index == obstacles.size - 1) 0 else index + 1
    val orderedObstacle = obstacles.sortWith((a,b) =>  ((a.position - center) /\) < ((b.position - center) /\))
    val joinedObstacle = join(orderedObstacle(index), orderedObstacle(nextIndex))

    if (joinedObstacle.isDefined){
      val newObstacleList = joinedObstacle.head +: (orderedObstacle diff List(orderedObstacle(index), orderedObstacle(nextIndex)))
      recursiveJoin(newObstacleList, center, index)
    } else recursiveJoin(orderedObstacle, center, index + 1)
  }

  def join(a:Obstacle, b:Obstacle): Option[Obstacle] = {
    assert(a.isInstanceOf[Food] == b.isInstanceOf[Food])
    if(a equals b)
      return Some(a)

    val newPointList = checkOverlap(a,b).toList
    if(newPointList.nonEmpty){
      val centroid = Vectors.findCentroid(newPointList)
      val ordered  = newPointList.sortWith((a,b) =>  ((a - centroid) /\) < ((b - centroid) /\))
      Some(Obstacle(ordered))
    } else None
  }

  def checkOverlap(a:Obstacle, b:Obstacle): Iterable[Vector2D] = {
    import model.environment.elements.EnvironmentElements._
    val freePointOfA = a.points.filter(p => !checkHasInside(b, p))
    val freePointOfB = b.points.filter(p => !checkHasInside(a, p))

    val overlappedPoint = a.points.filter(p => b.points.exists(p2 => p.~~(p2,1E-4)))

    if((freePointOfA.size < a.points.size)
      || (freePointOfB.size < b.points.size)
      || overlappedPoint.nonEmpty
      || (a.position --> b.position < math.min(a.maxDistanceFromCenter(), b.maxDistanceFromCenter()))) {
      freePointOfA.diff(overlappedPoint)++ freePointOfB
    } else {
      overlappedPoint.foreach(x => println(x))
      List.empty
    }
  }

//  def findOverlapAndJoin(obstacles: List[Obstacle], obstacle: Obstacle): List[Obstacle] = {
//    //Check that obstacle isn't inside the list
//    val removedObstacle =  obstacles.filter(o => !(o equals obstacle))
//    //find all obstacle could have an overlap
//    val nearObstacle = removedObstacle.filter(o => {
//      val centroidDist = o.position --> obstacle.position
//      val maxDistanceFromCenter = math.max(o.maxDistanceFromCenter(), obstacle.maxDistanceFromCenter())
//      (centroidDist < maxDistanceFromCenter)
//    })
//    //if overlap isn't find check with other element of list
//    if(nearObstacle.isEmpty){
//      if(removedObstacle.headOption.nonEmpty) {
//        val newElement = removedObstacle.head
//        findOverlapAndJoin(removedObstacle, newElement)
//      } else
//        List()
//    } else {
//      // else join overlapped element
//      val t = for {i <- 1 until nearObstacle.size
//                   ob <- if(checkOverlap(nearObstacle(i - 1), nearObstacle(i)))
//                   obstacles =
//                   }
//
//      nearObstacle.foldRight(obstacle)(join(_,_).head)
//      val clearObstacle =  obstacles.filter(o => {
//        !nearObstacle.exists(o2 => o2 equals (o))
//      })
//      t +: clearObstacle
//    }
//  }


  val listValue = List(3, 4, 8) // TODO refactor magic number
  def randomValid: Int = listValue(scala.util.Random.nextInt(listValue.size))
}

case class IntersectionResult(intersectionPoint: Vector2D, angle: Double)
