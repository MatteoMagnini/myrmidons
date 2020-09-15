package model

import utility.Geometry.Vector2D

/**
 * This class represents a food source.
 * */
case class Food(override val position: Vector2D, quantity: Int)
  extends SimpleObstacle(position, math.log(quantity)*10, math.log(quantity)*10) {

  /**
   * Increase food quantity.
   *
   * @param newQuantity to increase at actual quantity
   *
   * @return new instance of Food with increased quantity
   * */
  def + (newQuantity: Int): Food = {
    Food(position, quantity + newQuantity)
  }

  /**
   * Decrease food quantity.
   *
   * @param newQuantity to increase at actual quantity
   *
   * @return new instance of Food with decreased quantity
   * */
  def - (newQuantity: Int): Food = {
    this + ( - newQuantity)
  }
}
