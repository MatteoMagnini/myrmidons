package model

import utility.Geometry.Vector2D

/** A food source. */
case class Food(override val position: Vector2D, quantity: Double)
  extends SimpleObstacle(position, math.sqrt(quantity) *2 , math.sqrt(quantity) * 2 ) {

  /** Increase food quantity.
   *
   * @param newQuantity to increase actual quantity
   * @return new instance of Food with increased quantity
   * */
  def + (newQuantity: Double): Food = {
    Food(position, quantity + newQuantity)
  }

  /** Decrease food quantity.
   *
   * @param newQuantity to decrease actual quantity
   * @return new instance of Food with decreased quantity
   * */
  def - (newQuantity: Double): Food = {
   val dec : Double = (this + ( - newQuantity)).quantity
    if( dec < 1) this.copy(quantity = 0)
    else this.copy(quantity = dec)
  }
}
