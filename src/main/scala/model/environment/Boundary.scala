package model.environment

import utility.Geometry.Vector2D

/** Environment boundary */
trait Boundary {

  def topLeft: Vector2D
  def topRight: Vector2D
  def bottomLeft: Vector2D
  def bottomRight: Vector2D

  /** Returns the center of boundary */
  def center:Vector2D

  /** Returns whether a vector is inside boundaries */
  def hasInside(pos: Vector2D): Boolean
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
  private[this] class BoundaryImpl(left: Double, top: Double, width: Int, height: Int) extends Boundary {
    import utility.Geometry.TupleOp._

    override def topLeft: Vector2D = (left, top)
    override def topRight: Vector2D = (left + width, top)
    override def bottomLeft: Vector2D = (left, top + height)
    override def bottomRight: Vector2D = (left + width, top + height)

    override def center: Vector2D = (left + width / 2, top + height / 2)

    override def hasInside(pos: Vector2D): Boolean = {
      (pos.x >= left) && (pos.x <= topRight.x) &&
        (pos.y >= top) && (pos.y <= bottomLeft.y)
    }
  }
}
