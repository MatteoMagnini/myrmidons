package model

import utility.Geometry.Vector2D
import utility.Geometry.TupleOp3._
/** A food source. */
case class Food(override val position: Vector2D, quantity: Double)
  extends SimpleObstacle(position, if (math.sqrt(quantity) < 20) 20 else math.sqrt(quantity),
    if (math.sqrt(quantity) < 20) 20 else math.sqrt(quantity)) {

//  override def hasInside(coordinate: Vector2D): Boolean = {
//    math.pow(coordinate.x - position.x, 2) + math.pow(coordinate.y - position.y, 2) <  quantity/4
//  }

  /** Increase food quantity.
   *
   * @param newQuantity to increase actual quantity
   * @return new instance of Food with increased quantity
   **/
  def +(newQuantity: Double): Food = {
    Food(position, quantity + newQuantity)
  }

  /** Decrease food quantity.
   *
   * @param newQuantity to decrease actual quantity
   * @return new instance of Food with decreased quantity
   **/
  def -(newQuantity: Double): Food = {
    val dec: Double = (this + (-newQuantity)).quantity
    if (dec < 1) this.copy(quantity = 0)
    else this.copy(quantity = dec)
  }
}
