package model

import scala.language.implicitConversions

trait Obstacle{
  def isInside(tuple2: (Double, Double, Double)):Boolean
}

class EnvObstacle(val points: List[(Double, Double, Double)]) extends Obstacle {
  import HomogeneousConversion._
  var segments: List[((Double, Double, Double), (Double, Double, Double), (Double, Double, Double))] = List()

  if(points.size < 3){
    throw new IllegalArgumentException("points list must have more than 2 elements")
  }
  //get a line given two points
  points.indices foreach (i => {
    var before = if ( i == 0 ) points.length - 1 else i - 1
    val product = crossProduct(points(before), points(i))
    val line = (product._1/product._3, product._2/product._3, product._3/product._3)
    segments ::= (points(before), points(i), line)
  })

  override def isInside(tuple2: (Double, Double, Double)): Boolean = {
    var maxX = points.sortWith((a,b) => a._1 > b._1) head
    //track an ray in right version
    val ray = crossProduct(tuple2, (maxX._1 + 1, tuple2._2, 1))
    var counter = 0
    //find intersection between polygon segment and ray
    segments.indices foreach(i => {
      val crossIntersection = crossProduct(segments(i)._3, ray)
      // intersection at ideal point (parallel line)
      if (crossIntersection._3 != 0.0) {
        val intersection = (crossIntersection._1 / crossIntersection._3,
                            crossIntersection._2 / crossIntersection._3,
                            crossIntersection._3 / crossIntersection._3)
        if ((((segments(i)._1._1 >= intersection._1) && (segments(i)._2._1 <= intersection._1))
          || ((segments(i)._1._1 <= intersection._1) && (segments(i)._2._1 >= intersection._1)))
          && intersection._1 >= tuple2._1) {
          counter += 1
        }
      }
    })
    (counter % 2) != 0
  }

  def crossProduct(t1: (Double, Double, Double),t2: (Double, Double, Double)): (Double, Double, Double) = {
    val x = (t1._2 * t2._3) - (t1._3 * t2._2)
    val y = (t1._3 * t2._1) - (t1._1 * t2._3)
    val z = (t1._1 * t2._2) - (t1._2 * t2._1)
    (x, y, z)
  }
}

// implicit to accept tuple2
object HomogeneousConversion{
  implicit def tuple2ToTuple3(t: (Double, Double)): (Double, Double, Double) = {
    convT2ToT3(t)
  }
  implicit def listT2ToListT3(lt: List[(Double, Double)]): List[(Double, Double, Double)] ={
    var temp: List[(Double, Double, Double)] = List()
    lt foreach ( temp ::= convT2ToT3(_))
    temp
  }

  def convT2ToT3 (t: (Double,Double)): (Double, Double, Double) = {
    (t._1,t._2,1)
  }
}
