package model

/** A vector in 2-dimensional space.
 *
 * @param x x-coordinate
 * @param y y-coordinate
 * @param z z-coordinate
 */
case class Vector3D(x: Double, y: Double, z: Double) {
  import TupleOp3._

  /** Returns a vector in its opposite form */
  def - : Vector3D = (-x, -y, -z)

  /** Returns a vector shifted of vector delta */
  def >> (delta: Vector3D): Vector3D = (x + delta.x, y + delta.y, z + delta.z)

  /** Returns a vector subtraction */
  def - (delta: Vector3D): Vector3D = (x - delta.x, y - delta.y, z - delta.z)

  /** Returns a vector multiplied bby a constant */
  def * (s: Double): Vector3D = (s * x, s * y, s * z)

  /** Returns the norm of the vector */
  def || : Double = math.sqrt(x * x + y * y)

  /** Return the distance between vectors */
  def --> (other: Vector3D) : Double = this - other ||
}


/** Implicit conversions for [[model.Vector2D]] instances
 *
 * {{{
 * import TupleOp._
 * val v = (1, 2) >> (3, 4)
 * }}}
 */
object TupleOp3 {
  implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)
  implicit def intToVec3D(value: (Int, Int, Int)): Vector3D = Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)

  implicit def vec3DToVec2D(value: Vector2D): Vector3D = Vector3D(value.x, value.y, 1.0)
}
