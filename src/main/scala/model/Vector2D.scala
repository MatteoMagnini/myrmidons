package model

/** A vector in 2-dimensional space.
  *
  * @param x x-coordinate
  * @param y y-coordinate
  */
case class Vector2D(x: Double, y: Double) {
  import TupleOp._

  /** Returns a vector in its opposite form */
  def - : Vector2D = (-x, -y)

  /** Returns a vector shifted of vector delta */
  def >> (delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

  /** Returns a vector subtraction */
  def - (delta: Vector2D): Vector2D = (x - delta.x, y - delta.y)

  /** Returns a vector multiplied bby a constant */
  def * (s: Double): Vector2D = (s * x, s * y)

  /** Returns the norm of the vector */
  def || : Double = math.sqrt(x * x + y * y)

  /** Return the distance between vectors */
  def --> (other: Vector2D) : Double = this - other ||
}


/** Implicit conversions for [[model.Vector2D]] instances
  *
  * {{{
  * import TupleOp._
  * val v = (1, 2) >> (3, 4)
  * }}}
  */
object TupleOp {
  implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)
  implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)

  implicit def vec3DToVec2D(value: Vector3D): Vector2D = Vector2D(value.x, value.y)
}
