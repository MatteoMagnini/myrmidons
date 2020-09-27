package model.environment

import utility.Geometry.Vector2D

/** Environment boundary */
trait Boundary {
  import utility.Geometry.TupleOp._

  def top: Double
  def left: Double
  def width: Double
  def height: Double

  /** Four corners of boundary */
  def topLeft: Vector2D = (left, top)
  def topRight: Vector2D = (left + width, top)
  def bottomLeft: Vector2D = (left, top + height)
  def bottomRight: Vector2D = (left + width, top + height)

  /** Returns the center of boundary */
  def center: Vector2D = (left + width / 2, top + height / 2)

  /** Returns whether a vector is inside boundaries */
  def hasInside(pos: Vector2D): Boolean = {
    (pos.x >= topLeft.x) && (pos.x <= topRight.x) &&
      (pos.y >= topLeft.y) && (pos.y <= bottomLeft.y)
  }
}

object Boundary {

  def apply(left: Double, top: Double, width: Int, height: Int): Boundary =
    new BoundaryImpl(left, top, width, height)

  /** Environment boundary implementation
    *
    * @param left x-coordinate of top left corner
    * @param top y-coordinate of top left corner
    * @param width environment width
    * @param height environment height
    */
  private[this] class BoundaryImpl(override val left: Double, override val top: Double,
                                   override val width: Double, override val height: Double) extends Boundary
}
