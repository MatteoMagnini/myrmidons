package model.environment.pheromones

import common.geometry.Vector2D

import scala.reflect.ClassTag

/** Manage pheromone operation.
 *
 * @tparam A pheromone type
 */
class PheromoneOperationSolver[A <: Pheromone] {

  def decrease(constructor: (Vector2D, Double => Double, Double) => A, pheromone: A): Option[A] =
    if (pheromone.decreasingFunction(pheromone.intensity) <= 0) {
      None
    } else {
      Some(constructor(pheromone.position, pheromone.decreasingFunction,
        pheromone.decreasingFunction(pheromone.intensity)))
    }

  def merge[A <: Pheromone: ClassTag](constructor: (Vector2D, Double => Double, Double) => A, pheromone: A,
                                      otherPheromone: Pheromone, threshold: Double = 1E-10,
                                      intensity: Double): Option[A] = otherPheromone match {
    case p:A =>
      if (pheromone.position --> p.position > threshold) {
        None
      }
      else {
        Some(constructor(pheromone.position, pheromone.decreasingFunction, intensity + p.intensity))
      }

    case _ => None
  }
}
