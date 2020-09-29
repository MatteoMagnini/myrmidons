package model.environment.elements

import model.Drawable
import utility.geometry.{RandomVector2DInSquare, Vector2D}

/**A food source.
  *
  * @param position position in environment
  * @param quantity quantity of food
  */
case class Food(override val position: Vector2D, quantity: Double) extends Drawable {

  def radius: Double = if (math.sqrt(quantity) < 20) 20 else math.sqrt(quantity)

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
    val dec: Double = (this + (- newQuantity)).quantity
    if (dec < 1) this.copy(quantity = 0)
    else this.copy(quantity = dec)
  }
}

/**Factory methods */
object Food {
  def createRandomFood(minPos: Double = 0, maxPos: Double = 800, quantity: Int = 500): Food= {
    val pos = RandomVector2DInSquare(minPos, maxPos)
    Food(Vector2D(pos.x, pos.y), quantity)
  }
}
