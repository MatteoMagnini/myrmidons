package model.environment.pheromones

import model.Drawable

/**
 * Abstraction for a real pheromone.
 * A pheromone has a position, it is drawable.
 * It has also an intensity that decreases over time due to evaporation.
 */
trait Pheromone extends Drawable {

  /**
   * The rule for decreasing intensity over time.
   *
   * @return the new intensity
   */
  def decreasingFunction: Double => Double

  /**
   * @return the intensity of the pheromone.
   */
  def intensity: Double

  /**
   * Apply the decreasing function.
   *
   * @return the new pheromone if it is not completely evaporated
   */
  def decrease: Option[Pheromone]

  /**
   * Attempt to merge two pheromones if they are very close and are of the same type.
   *
   * @param pheromone the second pheromone
   * @param threshold the threshold under which apply the merge
   * @return Some of the new pheromone if they merge, none otherwise
   */
  def merge(pheromone: Pheromone, threshold: Double = 1E-10): Option[Pheromone]

}
