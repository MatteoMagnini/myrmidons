package model.environment

import model.Drawable
import utility.Geometry.Vector2D

/**
 * Abstraction for a real pheromone.
 * A pheromone has a position, it is drawable.
 * It has also an intensity that decreases over time due to evaporation.
 */
trait Pheromone extends Drawable {

  def DELTA: Double

  def intensity: Double

  def decrease: Option[Pheromone]

  /**
   * Attempt to merge two pheromones if they are very close and are of the same type
   *
   * @param pheromone the second pheromone
   * @param threshold the threshold under which apply the merge
   * @return Some of the new pheromone if they merge, none otherwise
   */
  def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[Pheromone]

}

object FoodPheromoneInfo {

  def DELTA: Double = 0.8

  def STARTING_INTENSITY: Double = 100.0

  def MAX_INTENSITY: Double = 1000.0

  def INTENSITY_FACTOR: Double = 3.0

}

case class FoodPheromone(override val position: Vector2D,
                         override val DELTA: Double,
                         override val intensity: Double) extends Pheromone {

  override def decrease: Option[FoodPheromone] =
    if (intensity - DELTA <= 0) None else Some(FoodPheromone(position, DELTA, intensity - DELTA))

  override def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[FoodPheromone] = pheromone match {
    case p: FoodPheromone =>
      if (position --> p.position > threshold) None
      else Some(FoodPheromone(position, DELTA, this.intensity + p.intensity))

    case _ => None
  }

}

object FoodPheromone {

  import FoodPheromoneInfo.MAX_INTENSITY

  def apply(position: Vector2D, DELTA: Double, intensity: Double): FoodPheromone =
    new FoodPheromone(position, DELTA, if (intensity > MAX_INTENSITY) MAX_INTENSITY else intensity)
}