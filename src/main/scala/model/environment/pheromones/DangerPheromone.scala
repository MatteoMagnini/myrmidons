package model.environment.pheromones
import utility.geometry.Vector2D

case class DangerPheromone(override val position: Vector2D,
                           override val decreasingFunction: Double => Double,
                           override val intensity: Double) extends Pheromone {

  override def decrease: Option[DangerPheromone] =
    if (decreasingFunction(intensity) <= 0) None else Some(DangerPheromone(position, decreasingFunction, decreasingFunction(intensity)))

  override def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[DangerPheromone] = pheromone match {
    case p: DangerPheromone =>
      if (position --> p.position > threshold) None
      else Some(DangerPheromone(position, decreasingFunction, this.intensity + p.intensity))

    case _ => None
  }

}

object DangerPheromone {
  import DangerPheromoneInfo._
  def apply(position: Vector2D, decreasingFunction: Double => Double, intensity: Double): DangerPheromone =
    new DangerPheromone(position, decreasingFunction, if (intensity > MAX_INTENSITY) MAX_INTENSITY else intensity)
}