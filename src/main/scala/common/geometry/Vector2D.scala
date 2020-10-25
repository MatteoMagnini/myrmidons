package common.geometry

import common.geometry.Vectors._

/** A vector in 2-dimensional space.
 *
 * @param x x-coordinate
 * @param y y-coordinate
 */
case class Vector2D(x: Double, y: Double) {

  /** Returns a vector in its opposite form */
  def - : Vector2D = (-x, -y)

  /** Returns a vector shifted of vector v */
  def >>(delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

  /** Returns a vector subtraction */
  def -(delta: Vector2D): Vector2D = this >> (delta -)

  /** Returns a vector multiplied by a constant */
  def *(s: Double): Vector2D = (x * s, y * s)

  /** Returns a vector multiplied by a constant */
  def /(s: Double): Vector2D = (x / s, y / s)

  /** Returns the norm of the vector */
  def || : Double = math.sqrt(x * x + y * y)

  /** Returns vector orientation in angle */
  def /\ : Double = math.atan2(y, x)

  /** Returns the angle between two vectors */
  def ^(s: Vector2D): Double = {
    val numeratorCos = (s.x * x) + (s.y * y)
    val denominator = (this ||) * (s ||)
    math.acos(numeratorCos / denominator)
  }

  /** Return the distance between vectors */
  def -->(other: Vector2D): Double = (this - other) ||

  /** Returns vectors equality using a default precision value */
  def ~~(other: Vector2D, precision: Double = 3): Boolean = ~=(x, other.x, precision) && ~=(y, other.y, precision)

  override def equals(obj: Any): Boolean = obj match {
    case o: Vector2D => ~=(x, o.x) && ~=(y, o.y)
    case _ => false
  }

  override def hashCode(): Int = super.hashCode()

}



