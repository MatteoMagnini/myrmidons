package model

import utility.Geometry.{Vector, Vector2D}


case class Food(position: Vector2D, quantity: Int)
  extends SimpleObstacle(position, quantity, quantity) {
}
