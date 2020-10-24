package model.environment.pheromones
import common.geometry.Vector2D

/**
 * A pheromone indicating danger for an ant.
 *
 * @param position of the pheromone
 * @param decreasingFunction simulating evaporation over time
 * @param intensity of the pheromone
 */
case class DangerPheromone(override val position: Vector2D,
                           override val decreasingFunction: Double => Double,
                           override val intensity: Double) extends Pheromone {
  private val engine = new PheromoneOperationSolver[DangerPheromone]()

  override def decrease: Option[DangerPheromone] =
    engine.decrease(DangerPheromone.apply, this)

  override def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[DangerPheromone] =
    engine.merge(DangerPheromone.apply, this, pheromone, threshold, intensity)

}

object DangerPheromone {
  import DangerPheromoneInfo._
  def apply(position: Vector2D, decreasingFunction: Double => Double, intensity: Double): DangerPheromone =
    new DangerPheromone(position, decreasingFunction, if (intensity > MAX_INTENSITY) MAX_INTENSITY else intensity)
}
