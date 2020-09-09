package model

import utility.Geometry.Vector

case class Boundary(left: Double, top: Double, width: Double, height: Double) {
  import utility.Geometry.TupleOp._

  def topLeft: Vector = (left, top)
  def topRight: Vector = (left + width, top)
  def bottomLeft: Vector = (left, top + height)
  def bottomRight: Vector = (left + width, top + height)

  def center: Vector = (left + width / 2, top + height / 2)

  def hasInside(pos: Vector): Boolean = {
    (pos.x >= left) && (pos.x <= topRight.x) &&
      (pos.y >= top) && (pos.y <= bottomLeft.y)
  }
}

object Boundary{
  def apply(left: Double, top: Double, width: Double, height: Double): Boundary =
    new Boundary(left, top, width, height)
}
