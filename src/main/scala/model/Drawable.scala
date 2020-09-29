package model

import utility.geometry.Vector2D

/**
 * All entities that can be displayed have a state that extends the Drawable state.
 * A Drawable has a position described as a Vector2D.
 */
trait Drawable {
  val position: Vector2D
  //type Position
}
