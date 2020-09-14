package model

import utility.Geometry.Vector2D

case class Food(override val position: Vector2D, quantity: Int)
  extends SimpleObstacle(position, quantity, quantity) {

  def + (newQuantity: Int): Food = {
    Food(position, quantity + newQuantity)
  }

  def - (newQuantity: Int): Food = {
    this + ( - newQuantity)
  }
}
