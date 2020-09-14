package model

import utility.Geometry.Vector2D

case class Food(override val position: Vector2D, quantity: Int)
  extends SimpleObstacle(position, quantity, quantity) {
}
