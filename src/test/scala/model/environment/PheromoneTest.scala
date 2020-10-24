package model.environment

import common.geometry.Vector2D
import common.geometry.Vector2DFactory.ZeroVector2D
import model.environment.pheromones.FoodPheromone
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike

class PheromoneTest extends AnyWordSpecLike with BeforeAndAfter {

  "A pheromone" when {

    val threshold = 1.0
    val DELTA = 1.0
    val decreasingFunction: Double => Double = x => x - DELTA
    val startingIntensity = 100.0

    "created" should {

      val pos1 = Vector2D(2.5,7.2)
      val p1 = FoodPheromone(pos1, decreasingFunction, startingIntensity)

      "be correctly initialized" in {
        assert(p1.position == pos1)
        assert(p1.intensity == startingIntensity)
      }

      val p2 = p1.decrease

      "decrease intensity" in {
        assert(p2.nonEmpty)
        assert(p2.get == FoodPheromone(pos1, decreasingFunction, startingIntensity - DELTA))
      }

      val pos2 = Vector2D(3,7)
      val p3 = FoodPheromone(pos2, decreasingFunction, startingIntensity)
      val p4 = p2.get.merge(p3,threshold)

      "merge with near pheromone" in {
        assert(p4.nonEmpty)
        assert(p4.get.position == pos1)
        assert(p4.get.intensity == 2 * startingIntensity - DELTA)
      }


      val p5 = FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity)
      val p6 = p4.get.merge(p5, threshold)

      "no merge if pheromones are far" in {
        assert(p6.isEmpty)
      }

    }
  }
}
