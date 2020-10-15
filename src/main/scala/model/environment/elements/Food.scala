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
    Food(position,
      quantity + newQuantity,
      Obstacle(position,
        Food.radius(math.round(quantity + newQuantity).toInt),
        points.size)
    )
  }

  /** Decrease food quantity.
   *
   * @param newQuantity to decrease actual quantity
   * @return new instance of Food with decreased quantity
   **/
  def -(newQuantity: Double): Food = {
    if(quantity - newQuantity <= 0) {
      this.copy(quantity = 0)
    } else {
      this + (- newQuantity)
    }
  }
}

/**Factory methods */
object Food {
  import model.environment._
    def createRandomFood(position: Vector2D,
                         minRadius:Double,
                         maxRadius:Double,
                         quantity: Int = FOOD_MIN_QUANTITY)
    : Food = {
      val pos = RandomVector2DInCircle(minRadius, maxRadius, position)
      Food(Vector2D(pos.x, pos.y), quantity, Obstacle(pos, radius(quantity), FOOD_VERTEX))
    }

  def apply(position: Vector2D, quantity: Double, o: Obstacle): Food = new Food(position, quantity, o)

  def apply(position: Vector2D, quantity: Double): Food =
    Food(position, quantity, Obstacle(position, radius(quantity.toInt), FOOD_VERTEX))

    def radius(quantity: Int): Double = if (math.sqrt(quantity) < FOOD_MIN_SIZE) FOOD_MIN_SIZE else math.sqrt(quantity)
}
