package utility.geometry

object Vectors {

  /** Double equivalence with a certain precision check.
    *
    * @param x default value
    * @param y value to check
    * @param precision to consider in number matching
    * @return true if the value are similar, otherwise false
    **/
  def ~=(x: Double, y: Double, precision: Double = 1E-1): Boolean = (x - y).abs < precision

  /** Returns a random value in a range
    *
    * @param min lower bound of range
    * @param max upper bound of range
    * @return a random value
    */
  def doubleInRange(min: Double, max: Double): Double =
    min + (max - min) * scala.util.Random.nextDouble()

  /**
   * This function return the intersection point of two line.
   *
   * @param seg1 (startSegPoint, stopSegPoint, lineVector)
   * @param seg2 (startSegPoint, stopSegPoint, lineVector)
   *
   * @return an Option[Vector2D] if the intersection point of
   *         the two segment are found, otherwise return None
   * */
  def findIntersectionPoint(seg1: (Vector2D, Vector2D, Vector3D),
                                    seg2: (Vector2D, Vector2D, Vector3D)): Option[Vector2D] = {
    val crossIntersection = seg1._3 X seg2._3
    if (!(Vectors ~= (crossIntersection.z, 0.0, 1E-7))) {
      val intersection = crossIntersection / crossIntersection.z
      if ((intersection checkInside(seg1._1, seg1._2))
        && (intersection checkInside(seg2._1, seg2._2))) {
        Some(intersection)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
   * This function return the angle (in radians) formed by segment
   * line intersection.
   *
   * @param seg1 (startSegPoint, stopSegPoint, lineVector)
   * @param seg2 (startSegPoint, stopSegPoint, lineVector)
   *
   * @return angle
   * */
  def findIntersectionAngle(seg1: (Vector2D, Vector2D, Vector3D),
                                    seg2: (Vector2D, Vector2D, Vector3D)): Double = {
    val seg1Vector: Vector2D =  seg1._2 - seg1._1
    val seg2Vector: Vector2D =  seg2._2 - seg2._1
    seg2Vector ^ seg1Vector
  }

  // a segments is described as a two point and a line pass through them
  def findCentroid(l: List[Vector2D]): Vector2D = {
    l.foldRight(Vector2D(0.0, 0.0))(_ >> _) / l.size
  }

}
