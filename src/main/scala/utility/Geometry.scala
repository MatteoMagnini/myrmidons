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
    def mod: Double
    def dist(p1: Vector): Double

    override def toString: String = LEFT + x + SEPARATOR + y + RIGHT
  }

  case class Vector2D( override val x: Double, override val y: Double) extends Vector {

    override def +( p1: Vector ): Vector = Vector2D(x + p1.x, y + p1.y)

    override def -(p1: Vector): Vector = Vector2D(x - p1.x, y - p1.y)

    override def mod: Double = math.sqrt(x*x + y*y)

    override def dist(p1: Vector): Double = this - p1 mod
  }



}
