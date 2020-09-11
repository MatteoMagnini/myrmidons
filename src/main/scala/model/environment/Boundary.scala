package model.environment

import utility.Geometry.Vector

/** Environment boundary
  *
  * @param left x-coordinate of top left corner
  * @param top y-coordinate of top left corner
  * @param width environment width
  * @param height environment height
  */
case class Boundary(left: Double, top: Double, width: Int, height: Int) {
  import utility.Geometry.TupleOp._

  def topLeft: Vector = (left, top)
  def topRight: Vector = (left + width, top)
  def bottomLeft: Vector = (left, top + height)
  def bottomRight: Vector = (left + width, top + height)

  /** Center of boundary */
  def center: Vector = (left + width / 2, top + height / 2)

  /** Returns whether a vector is inside boundaries */
  def hasInside(pos: Vector): Boolean = {
    (pos.x >= left) && (pos.x <= topRight.x) &&
      (pos.y >= top) && (pos.y <= bottomLeft.y)
  }
}

object Boundary {
  def apply(left: Double, top: Double, width: Int, height: Int): Boundary =
    new Boundary(left, top, width, height)
}
