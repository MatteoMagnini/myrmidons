package model

case class Vector2D(x: Double, y: Double) {
  import TupleOp._

  def >> (delta: Vector2D): Vector2D = (x + delta.x, y + delta.y)

  def - : Vector2D = (-x, -y)

  def - (delta: Vector2D): Vector2D = (x - delta.x, y - delta.y)

  def * (s: Int): Vector2D = (s * x, s * y)

  def || : Double = math.sqrt(x * x + y * y)

  def --> (other: Vector2D) : Double = this - other ||
}

case object ZeroVec2D {
  def apply: Vector2D = Vector2D(0, 0)
}


object TupleOp {
  implicit def toVec2D(value: (Double, Double)): Vector2D = Vector2D(value._1, value._2)
  implicit def intToVec2D(value: (Int, Int)): Vector2D = Vector2D(value._1.toDouble, value._2.toDouble)
}
