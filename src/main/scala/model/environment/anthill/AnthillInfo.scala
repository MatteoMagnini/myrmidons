package model.environment.anthill

import common.geometry.Vector2D
import model.Drawable

/** Anthill state.
 *
 * @param position      of the anthill
 * @param radius        of the anthill
 * @param foodAmount    inside the anthill
 * @param maxFoodAmount that an anthill can have
 */
case class AnthillInfo(override val position: Vector2D,
                       radius: Double,
                       foodAmount: Double,
                       maxFoodAmount: Double) extends Drawable {


  def incrementFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount + delta > maxFoodAmount) maxFoodAmount else foodAmount + delta)

  def decrementFood(delta: Double): AnthillInfo =
    this.copy(foodAmount = if (foodAmount - delta < 0) 0 else foodAmount - delta)
}

object AnthillInfo {
  def apply(position: Vector2D, radius: Double = 3, foodAmount: Double = 0,
            maxFoodAmount: Double = MAX_FOOD_AMOUNT): AnthillInfo =
    new AnthillInfo(position, radius, foodAmount, maxFoodAmount)
}
