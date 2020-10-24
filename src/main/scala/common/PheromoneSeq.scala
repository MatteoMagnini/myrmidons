package common

import common.geometry.Vector2D
import common.geometry.Vector2DFactory.ZeroVector2D
import model.environment.pheromones.Pheromone

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
