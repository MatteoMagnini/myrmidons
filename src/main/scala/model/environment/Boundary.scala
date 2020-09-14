package model.environment

import utility.Geometry.Vector2D

/** Environment boundary
  *
  * @param left x-coordinate of top left corner
  * @param top y-coordinate of top left corner
  * @param width environment width
  * @param height environment height
  */
case class Boundary(left: Double, top: Double, width: Int, height: Int) {
  import utility.Geometry.TupleOp._

  def topLeft: Vector2D = (left, top)
  def topRight: Vector2D = (left + width, top)
  def bottomLeft: Vector2D = (left, top + height)
  def bottomRight: Vector2D = (left + width, top + height)

  /** Center of boundary */
  def center: Vector2D = (left + width / 2, top + height / 2)

  /** Returns whether a vector is inside boundaries */
  def hasInside(pos: Vector2D): Boolean = {
    (pos.x >= left) && (pos.x <= topRight.x) &&
      (pos.y >= top) && (pos.y <= bottomLeft.y)
  }
}

object Boundary {
  def apply(left: Double, top: Double, width: Int, height: Int): Boundary =
    new Boundary(left, top, width, height)
}
