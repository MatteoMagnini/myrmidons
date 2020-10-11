package model.environment.pheromones

import utility.geometry.Vector2D

/**
 * A foraging ant releases it when carrying food to the anthill.
 * When searching for food, a foraging ant follows it.
 *
 * @param position of the pheromone
 * @param decreasingFunction TODO: should become a function
 * @param intensity the current intensity value
 */
case class FoodPheromone(override val position: Vector2D,
                         override val decreasingFunction: Double => Double,
                         override val intensity: Double) extends Pheromone {

  override def decrease: Option[FoodPheromone] =
    if (decreasingFunction(intensity) <= 0) None else Some(FoodPheromone(position, decreasingFunction, decreasingFunction(intensity)))

  override def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[FoodPheromone] = pheromone match {
    case p: FoodPheromone =>
      if (position --> p.position > threshold) None
      else Some(FoodPheromone(position, decreasingFunction, this.intensity + p.intensity))

    case _ => None
  }

}

object FoodPheromone {

  import utility.Parameters.Pheromones.FoodPheromoneInfo.MAX_INTENSITY

  def apply(position: Vector2D, decreasingFunction: Double => Double, intensity: Double): FoodPheromone =
    new FoodPheromone(position, decreasingFunction, if (intensity > MAX_INTENSITY) MAX_INTENSITY else intensity)
}
