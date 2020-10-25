package model.environment.pheromones

import common.geometry.Vector2D

/**
 * A pheromone indicating the presence of a food source.
 *
 * @param position           of the pheromone
 * @param decreasingFunction simulating evaporation over time
 * @param intensity          the current intensity value
 */
case class FoodPheromone(override val position: Vector2D,
                         override val decreasingFunction: Double => Double,
                         override val intensity: Double) extends Pheromone {

  private val engine = new PheromoneOperationSolver[FoodPheromone]()

  override def decrease: Option[FoodPheromone] =
    engine.decrease(FoodPheromone.apply, this)

  override def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[FoodPheromone] =
    engine.merge(FoodPheromone.apply, this, pheromone, threshold, intensity)
}

object FoodPheromone {

  import FoodPheromoneInfo._

  def apply(position: Vector2D, decreasingFunction: Double => Double, intensity: Double): FoodPheromone =
    new FoodPheromone(position, decreasingFunction, if (intensity > MAX_INTENSITY) MAX_INTENSITY else intensity)
}
