package utility.geometry
import utility.geometry.Vectors._

/** A vector in 3-dimensional space.
  *
  * @param x x-coordinate
  * @param y y-coordinate
  * @param z z-coordinate
  */
case class Vector3D(x: Double, y: Double, z: Double) {

  import TupleOp3._
  def - : Vector3D = (-x, -y, -z)

  def >>(delta: Vector3D): Vector3D = (x + delta.x, y + delta.y, z + delta.z)

  def -(delta: Vector3D): Vector3D = this >> (delta -)

  def *(s: Double): Vector3D = (s * x, s * y, s * z)

  def /(s: Double): Vector3D = (x / s, y / s, z / s)

  def || : Double = math.sqrt(x * x + y * y + z * z)

  def /\ : Double = math.atan(y / x)

  def ^ (s: Vector3D) : Double = {
    val numeratorCos = (s.x * this.x) + (s.y * this.y) + (s.z * this.z)
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
    * @param stop second point
    *
    * @return true if this vector is inside start and stop vector, otherwise return false
    *
    * */
  def checkInside(start: Vector3D, stop: Vector3D): Boolean = {
    ~=((start --> this) + (stop --> this) - (start --> stop), 0.0, 1E-7)
  }


  /** Cross product between two vectors * */
  def X(other: Vector3D): Vector3D = {
    val x = (this.y * other.z) - (this.z * other.y)
    val y = (this.z * other.x) - (this.x * other.z)
    val z = (this.x * other.y) - (this.y * other.x)
    (x, y, z)
  }
}

/** Implicit conversion to convert [[utility.geometry.Vector3D]] instances
  *
  * {{{
  * import TupleOp3._
  * val v = (1, 2, 1) >> (3, 4, 1)
  * }}}
  * */
object TupleOp3 {
  implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)

  implicit def intToVec3D(value: (Int, Int, Int)): Vector3D =
    Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)
}

