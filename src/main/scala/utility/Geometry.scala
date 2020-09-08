package utility

object Geometry {

  trait Vector {
    val LEFT = "["
    val RIGHT = "]"
    val SEPARATOR = ", "

    def x: Double
    def y: Double

    def +(p1: Vector): Vector
    def -(p1: Vector) : Vector
    def *(scalar: Double) : Vector
    def mod: Double
    def dist(p1: Vector): Double

    override def toString: String = LEFT + x + SEPARATOR + y + RIGHT

    override def equals( obj: Any ): Boolean = obj match {
      case v:Vector => (this.x equals v.x) && (this.y equals v.y)
      case _ => false
    }
  }

  case class Vector2D( override val x: Double, override val y: Double) extends Vector {

    override def +( p1: Vector ): Vector = Vector2D(x + p1.x, y + p1.y)

    override def -(p1: Vector): Vector = Vector2D(x - p1.x, y - p1.y)

    override def *(scalar: Double): Vector = Vector2D(scalar * this.x, scalar * this.y)

    override def mod: Double = math.sqrt(x*x + y*y)

    override def dist(p1: Vector): Double = this - p1 mod
  }

  object ZeroVector2D {
    def apply(): Vector = Vector2D(0,0)
  }


}
