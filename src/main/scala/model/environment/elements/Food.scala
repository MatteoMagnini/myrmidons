package model.environment.elements

import utility.geometry.{RandomVector2DInCircle, Vector2D}

/**A food source.
  *
  * @param position position in environment
  * @param quantity quantity of food
  */
case class Food(override val position: Vector2D, quantity: Double, o: Obstacle) extends Obstacle(o.points) {

  def radius: Double = o.position --> o.points.head

  /** Increase food quantity.
   *
   * @param newQuantity to increase actual quantity
   * @return new instance of Food with increased quantity
   **/
  def +(newQuantity: Double): Food = {
    Food(position, quantity + newQuantity, Obstacle(position, Food.radius(math.round(quantity + newQuantity).toInt),points.size))
  }

  /** Decrease food quantity.
   *
   * @param newQuantity to decrease actual quantity
   * @return new instance of Food with decreased quantity
   **/
  def -(newQuantity: Double): Food = {
    if(quantity - newQuantity <= 0)
      this.copy(quantity = 0)
    else
      this + (- newQuantity)
  }
}

/**Factory methods */
object Food {
    def createRandomFood(position: Vector2D, minRadius:Double, maxRadius:Double, quantity: Int = 500): Food = {
      val pos = RandomVector2DInCircle(minRadius, maxRadius, position)
      Food(Vector2D(pos.x, pos.y), quantity, Obstacle(pos, radius(quantity), 16))
    }

    def radius(quantity: Int): Double = if (math.sqrt(quantity) < 5) 5 else math.sqrt(quantity)
}
