package utility

object Geometry {

  /** A vector in 2-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   */
  case class Vector2D(x: Double, y: Double) {
    import TupleOp._

    /** Returns a vector in its opposite form */
    def - : Vector2D = (-x, -y)

    /** Returns a vector shifted of vector v */
    def >> (delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

    /** Returns a vector subtraction */
    def - (delta: Vector2D): Vector2D = this >> (delta -)

    /** Returns a vector multiplied by a constant */
    def * (s: Double): Vector2D = (s * x, s * y)

    /** Returns the norm of the vector */
    def || : Double = math.sqrt(x * x + y * y)

    /** Returns the angle of the vector */
    def /\ : Double = math.atan(y/x)

    /** Return the distance between vectors */
    def -->(other: Vector2D) : Double = this - other ||
  }

  /** Vector factory */
  object Vector2D {
    def apply(x: Double, y: Double): Vector2D = new Vector2D(x, y)
  }

  /** Zero vector factory */
  object ZeroVector2D {
    def apply(): Vector2D = Vector2D(0, 0)
  }

  /** Random vector factory */
  object RandomVector2D {

    def apply(min: Double, max: Double): Vector2D = {
      val uniformDoubleGenerator = scala.util.Random
      val x = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2.0
      val y = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2.0
      bound(min,max, Vector2D(x,y))
    }

    def apply(min: Double, max: Double, perturbation: Vector2D): Vector2D =
      bound(min,max, apply(min, max) >> perturbation)

    def apply(minX: Double, maxX: Double, minY: Double, maxY: Double ): Vector2D =
      Vector2D(doubleInRange(minX,maxX), doubleInRange(minY,maxY))

    private def bound(min: Double, max: Double, v: Vector2D): Vector2D =
      Vector2D(if (v.x.abs < min) v.x.signum * min else if (v.x.abs > max) v.x.signum * max else v.x,
        if (v.y.abs < min) v.y.signum * min else if (v.y.abs > max) v.y.signum * max else v.y)

    private def doubleInRange(min: Double, max: Double): Double =
      min + (max - min) * scala.util.Random.nextDouble()
  }

  /** Random vector factory */
  object OrientedVector2D {
    def apply(radiant: Double, module: Double ): Vector2D =
      Vector2D(math.sin(radiant) * module, math.cos(radiant) * module)
  }

  /** A vector in 3-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   * @param z z-coordinate
   */
 /* case class Vector3D(override val x: Double, override val y: Double, z: Double) extends Vector{
    import TupleOp._
    import VectorsOp._

    override def - : Vector3D = (-x, -y, -z)

    override def >> (delta: Vector): Vector3D = (x + delta.x, y + delta.y, z + delta.z)

    override def - (delta: Vector): Vector3D = (x - delta.x, y - delta.y, z - delta.z) //this >> (delta -)

    override def * (s: Double): Vector3D = (s * x, s * y, s * z)

    def / (s: Double): Vector3D = (x / s, y / s, z / s)

    override def || : Double = math.sqrt(x * x + y * y + z * z)

    override def /\ : Double = math.atan(y / x)

    override def --> (other: Vector) : Double = this - other ||

    /** Cross product between two vectors * */
    def X (other: Vector3D) : Vector3D = {
      val x = (this.y * other.z) - (this.z * other.y)
      val y = (this.z * other.x) - (this.x * other.z)
      val z = (this.x * other.y) - (this.y * other.x)
      (x, y, z)
    }
  }*/


  /** Implicit conversions for [[utility.Geometry.Vector2D]] instances
   *
   * {{{
   * import TupleOp._
   * val v = (1, 2) >> (3, 4)
   * }}}
   */
  object TupleOp {
    implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)
    implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)
  }

 /* /** Implicit conversion to convert [[utility.Geometry.Vector]] instances into
    * [[utility.Geometry.Vector3D]] ones.
    * */
  object VectorsOp {
    implicit def toVec3D(value: (Double, Double, Double)): Vector3D = Vector3D(value._1, value._2, value._3)
    implicit def intToVec3D(value: (Int, Int, Int)): Vector3D = Vector3D(value._1.toDouble, value._2.toDouble, value._3.toDouble)
    implicit def vec2DToVec3D(value: Vector): Vector3D = Vector3D(value.x, value.y, 1.0)
  }*/
}
