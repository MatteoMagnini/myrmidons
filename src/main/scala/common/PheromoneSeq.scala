package common

import model.environment.pheromones.Pheromone
import common.geometry.{Vector2D, ZeroVector2D}

/**
 * Pimp my library pattern.
 * A rich seq for pheromone objects.
 */
object PheromoneSeq {

  implicit class PheromoneSeq[A <: Pheromone](seq: Seq[A]) {

    /**
     * @param position of the ant
     * @return weighted sum vector
     */
    def weightedSum(position: Vector2D): Vector2D =
      if (seq.isEmpty) {
        ZeroVector2D()
      }
      else {
        seq.toStream.map(e => (e.position - position) * (e.intensity / (e.position --> position))).reduce(_ >> _)
      }
  }
}
