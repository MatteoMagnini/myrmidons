package utility

object Geometry {

  trait Vector {
    def x: Double
    def y: Double

    /** Returns a vector shifted of vector delta */
    def >>(p1: Vector): Vector
    /** Returns a vector in its opposite form */
    def - : Vector
    /** Returns a vector subtraction */
    def -(p1: Vector) : Vector
    /** Returns a vector multiplied bby a constant */
    def *(scalar: Double) : Vector
    /** Returns the norm of the vector */
    def || : Double
    /** Return the distance between vectors */
    def -->(p1: Vector): Double
  }

  /** A vector in 2-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   */
  case class Vector2D(override val x: Double, override val y: Double) extends Vector {
    import TupleOp._

    override def - : Vector = (-x, -y)

    override def >>(delta: Vector): Vector = (x + delta.x, y + delta.y)

    override def - (delta: Vector): Vector = (x - delta.x, y - delta.y)

    override def * (s: Double): Vector = (s * x, s * y)

    override def || : Double = math.sqrt(x * x + y * y)

    override def -->(other: Vector) : Double = this - other ||
  }

  /** A vector in 3-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   * @param z z-coordinate
   */
  case class Vector3D(override val x: Double, override val y: Double, z: Double) extends Vector{
    import TupleOp._

    def - : Vector = (-x, -y, -z)

    def >> (delta: Vector): Vector3D = (x + delta.x, y + delta.y, z + delta.asInstanceOf[Vector3D].z)

    def - (delta: Vector): Vector3D = (x - delta.x, y - delta.y, z - delta.asInstanceOf[Vector3D].z)

    def * (s: Double): Vector3D = (s * x, s * y, s * z)

    def / (s: Double): Vector3D = (x / s, y / s, z / s)

    def || : Double = math.sqrt(x * x + y * y + z * z)

    def --> (other: Vector) : Double = this - other ||

    /**
     * Cross product between two vector
     * */
    def X (other: Vector3D) : Vector3D = {
      val x = (this.y * other.z) - (this.z * other.y)
      val y = (this.z * other.x) - (this.x * other.z)
      val z = (this.x * other.y) - (this.y * other.x)
      (x, y, z)
    }
  }

  object ZeroVector2D {
    def apply(): Vector = Vector2D(0, 0)
  }

  object ZeroVector3D {
    def apply(): Vector = Vector3D(0, 0, 0)
  }

  object RandomVector2D {
    def apply(min: Double, max: Double): Vector = {
      val uniformDoubleGenerator = scala.util.Random
      val x = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2
      val y = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2
      Vector2D(if (x.abs < min) x.signum * min else x, if (y.abs < min) y.signum * min else y)
    }
  }

  /** Implicit conversions for [[utility.Geometry.Vector]] instances
   *
   * {{{
   * import TupleOp._
   * val v = (1, 2) >> (3, 4)
   * }}}
   */
  object TupleOp {
    implicit def toVec2D(value: (Double, Double)): Vector = Vector2D(value._1, value._2)
    implicit def intToVec2D(value: (Int, Int)): Vector = Vector2D(value._1.toDouble, value._2.toDouble)
    implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)
    implicit def intToVec3D(value: (Int, Int, Int)): Vector3D = Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)
    implicit def vec3DToVec2D(value: Vector2D): Vector3D = Vector3D(value.x, value.y, 1.0)
  }
}
