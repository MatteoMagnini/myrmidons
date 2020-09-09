package utility

object Geometry {

  trait Vector {
    val LEFT = "["
    val RIGHT = "]"
    val SEPARATOR = ", "

    def x: Double
    def y: Double

    def >>(p1: Vector): Vector
    def - : Vector
    def -(p1: Vector) : Vector
    def *(scalar: Double) : Vector
    def || : Double
    def -->(p1: Vector): Double

    override def toString: String = LEFT + x + SEPARATOR + y + RIGHT

    override def equals( obj: Any ): Boolean = obj match {
      case v:Vector => (this.x equals v.x) && (this.y equals v.y)
      case _ => false
    }
  }

  /** A vector in 2-dimensional space.
   *
   * @param x x-coordinate
   * @param y y-coordinate
   */
  case class Vector2D( x: Double, y: Double) extends Vector {
    import TupleOp._

    /** Returns a vector in its opposite form */
    override def - : Vector = (-x, -y)

    /** Returns a vector shifted of vector delta */
    override def >>(delta: Vector): Vector = (x + delta.x, y + delta.y)

    /** Returns a vector subtraction */
    override def - (delta: Vector): Vector = (x - delta.x, y - delta.y)

    /** Returns a vector multiplied bby a constant */
    override def * (s: Double): Vector = (s * x, s * y)

    /** Returns the norm of the vector */
    override def || : Double = math.sqrt(x * x + y * y)

    /** Return the distance between vectors */
    override def -->(other: Vector) : Double = this - other ||
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
  }

  object ZeroVector2D {
    def apply(): Vector = Vector2D(0,0)
  }

  object RandomVector2D {
    def apply(min: Double, max: Double): Vector = {
      val uniformDoubleGenerator = scala.util.Random
      val x = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2
      val y = (uniformDoubleGenerator.nextDouble() - 0.5) * max * 2
      Vector2D(if (x.abs < min) x.signum * min else x, if (y.abs < min) y.signum * min else y)
    }
  }

}
