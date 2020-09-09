package model

case class Boundary(left: Double, top: Double, width: Double, height: Double) {
  import TupleOp._

  def topLeft: Vector2D = (left, top)
  def topRight: Vector2D = (left + width, top)
  def bottomLeft: Vector2D = (left, top + height)
  def bottomRight: Vector2D = (left + width, top + height)

  def center: Vector2D = (left + width / 2, top + height / 2)

  def hasInside(pos: Vector2D): Boolean = {
    (pos.x >= left) && (pos.x <= topRight.x) &&
      (pos.y >= top) && (pos.y <= bottomLeft.y)
  }
}

object Boundary{
  def apply(left: Double, top: Double, width: Double, height: Double): Boundary =
    new Boundary(left, top, width, height)
}
