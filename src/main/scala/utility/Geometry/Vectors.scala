package utility.Geometry

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

}

/** Implicit conversions from [[utility.Geometry.Vector2D]] to [[utility.Geometry.Vector3D]] and vice versa. */
object VectorsImplicits {
  implicit def vec3DToVec2D(value: Vector3D): Vector2D = Vector2D(value.x / value.z, value.y / value.z)

  implicit def vec2DToVec3D(value: Vector2D): Vector3D = Vector3D(value.x, value.y, 1.0)
}
