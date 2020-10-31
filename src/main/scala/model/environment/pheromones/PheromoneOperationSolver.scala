package model.environment.pheromones

import common.geometry.Vector2D

import scala.reflect.ClassTag

/** Manage [[model.environment.pheromones.Pheromone]] operation.
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

  def merge[B <: Pheromone: ClassTag](constructor: (Vector2D, Double => Double, Double) => B, pheromone: B,
                                      otherPheromone: Pheromone, threshold: Double = 1E-10,
                                      intensity: Double): Option[B] = otherPheromone match {
    case p:B =>
      if (pheromone.position --> p.position > threshold) {
        None
      }
      else {
        Some(constructor(pheromone.position, pheromone.decreasingFunction, intensity + p.intensity))
      }

    case _ => None
  }
}
