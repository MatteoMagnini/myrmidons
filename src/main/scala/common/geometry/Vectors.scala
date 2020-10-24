package common.geometry

object Vectors {


  /** Equivalence with a certain precision check.
   *
   * @param x         default value
   * @param y         value to check
   * @param precision to consider in number matching
   * @return true if the value are similar, otherwise false
   * */
  def ~=(x: Double, y: Double, precision: Double = 1E-1): Boolean = (x - y).abs < precision

  /** Random value in range.
   *
   * @param min lower bound of range
   * @param max upper bound of range
   * @return a random value
   */
  def doubleInRange(min: Double, max: Double): Double =
    min + (max - min) * scala.util.Random.nextDouble()

  /** Find intersection point of two line.
   * TODO SIMO CAMBIA DESCRIZIONE SEGMENTI
   *
   * @param firstSegment  (startSegPoint, stopSegPoint, lineVector)
   * @param secondSegment (startSegPoint, stopSegPoint, lineVector)
   * @return intersection point if present
   *
   **/
  def findIntersectionPoint(firstSegment: (Vector2D, Vector2D, Vector3D),
                            secondSegment: (Vector2D, Vector2D, Vector3D)): Option[Vector2D] = {
    val crossIntersection = firstSegment._3 X secondSegment._3
    if (!(Vectors ~= (crossIntersection.z, 0.0, 1E-7))) {
      val intersection = crossIntersection / crossIntersection.z
      if ((intersection checkInside(firstSegment._1, firstSegment._2))
        && (intersection checkInside(secondSegment._1, secondSegment._2))) {
        Some(intersection)
      } else {
        None
      }
    } else {
      None
    }
  }

  /**
   * Angle between the two segment.
   *
   * @param firstSegment  (startSegPoint, stopSegPoint, lineVector)
   * @param secondSegment (startSegPoint, stopSegPoint, lineVector)
   * @return angle
   **/
  def findIntersectionAngle(firstSegment: (Vector2D, Vector2D, Vector3D),
                            secondSegment: (Vector2D, Vector2D, Vector3D)): Double = {
    val seg1Vector: Vector2D = firstSegment._2 - firstSegment._1
    val seg2Vector: Vector2D = secondSegment._2 - secondSegment._1
    seg2Vector ^ seg1Vector
  }

  /** The centroid of the shape.
   *
   * @param verticesList list of vertices position.
   * @return centroid
   */
  def findCentroid(verticesList: List[Vector2D]): Vector2D = {
    verticesList.foldRight(Vector2D(0.0, 0.0))(_ >> _) / verticesList.size
  }

}
