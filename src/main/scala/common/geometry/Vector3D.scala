package common.geometry

import common.geometry.Vectors._

/** A vector in 3-dimensional space.
  *
  * @param x x-coordinate
  * @param y y-coordinate
  * @param z z-coordinate
  */
case class Vector3D(x: Double, y: Double, z: Double) {

  def - : Vector3D = (-x, -y, -z)

  def >>(delta: Vector3D): Vector3D = (x + delta.x, y + delta.y, z + delta.z)

  def -(delta: Vector3D): Vector3D = this >> (delta -)

  def *(s: Double): Vector3D = (s * x, s * y, s * z)

  def /(s: Double): Vector3D = (x / s, y / s, z / s)

  def || : Double = math.sqrt(x * x + y * y + z * z)

  def /\ : Double = math.atan(y / x)

  def ^(s: Vector3D): Double = {
    val numeratorCos = (s.x * x) + (s.y * y) + (s.z * z)
    val denominator = (this ||) * (s ||)
    math.acos(numeratorCos / denominator)
  }

  def -->(other: Vector3D): Double = this - other ||

  /**
    * the distance between start point and intersection, summed distance to
    * intersection and stop point must be equals to distance between start point
    * and stop point.
    *
    * @param start first point
    * @param stop  second point
    * @return true if this vector is inside start and stop vector, otherwise return false
    *
    **/
  def checkInside(start: Vector3D, stop: Vector3D): Boolean = {
    ~=((start --> this) + (stop --> this) - (start --> stop), 0.0, 1E-7)
  }


  /** Cross product between two vectors.
    *
    * @param other vector 3D
    * @return cross product
    */
  def X(other: Vector3D): Vector3D = {
    val newX = (y * other.z) - (z * other.y)
    val newY = (z * other.x) - (x * other.z)
    val newZ = (x * other.y) - (y * other.x)
    (newX, newY, newZ)
  }
}


