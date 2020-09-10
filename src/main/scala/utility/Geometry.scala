package utility

object Geometry {

  trait Vector {
    def x: Double
    def y: Double

    /** Returns a vector shifted of vector v */
    def >>(v: Vector): Vector
    /** Returns a vector in its opposite form */
    def - : Vector
    /** Returns a vector subtraction */
    def -(v: Vector) : Vector
    /** Returns a vector multiplied by a constant */
    def *(scalar: Double) : Vector
    /** Returns the norm of the vector */
    def || : Double
    /** Returns the angle of the vector */
    def /\ : Double
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

    override def /\ : Double = math.atan(y/x)

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

    override def - : Vector = (-x, -y, -z)

    override def >> (delta: Vector): Vector3D = (x + delta.x, y + delta.y, z + delta.asInstanceOf[Vector3D].z)

    override def - (delta: Vector): Vector3D = (x - delta.x, y - delta.y, z - delta.asInstanceOf[Vector3D].z)

    override def * (s: Double): Vector3D = (s * x, s * y, s * z)

    def / (s: Double): Vector3D = (x / s, y / s, z / s)

    override def || : Double = math.sqrt(x * x + y * y + z * z)

    override def /\ : Double = math.atan(y/x)

    override def --> (other: Vector) : Double = this - other ||

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
      bound(min,max, Vector2D(x,y))
    }

    def apply(min: Double, max: Double, perturbation: Vector): Vector =
      bound(min,max, apply(min, max) >> perturbation)

    def apply(minX: Double, maxX: Double, minY: Double, maxY: Double ): Vector =
      Vector2D(doubleInRange(minX,maxX), doubleInRange(minY,maxY))

    private def bound(min: Double, max: Double, v: Vector): Vector =
      Vector2D(if (v.x.abs < min) v.x.signum * min else v.x, if (v.y.abs < min) v.y.signum * min else v.y)

    private def doubleInRange(min: Double, max: Double): Double =
      min + (max - min) * scala.util.Random.nextDouble()
  }

  object OrientedVector2D {
    def apply(radiant: Double, module: Double ): Vector =
      Vector2D(math.sin(radiant) * module, math.cos(radiant) * module)
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
    implicit def vec3DToVec2D(value: Vector): Vector3D = Vector3D(value.x, value.y, 1.0)
  }
}
