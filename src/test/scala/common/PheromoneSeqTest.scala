package common

import model.environment.pheromones.FoodPheromone
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import common.geometry.{Vector2D, ZeroVector2D}

class PheromoneSeqTest extends AnyWordSpecLike with BeforeAndAfter {

  import common.PheromoneSeq._

  "A seq of pheromones" when {

    val DELTA = 1.0
    val decreasingFunction: Double => Double = x => x - DELTA
    val startingIntensity = 10.0

    "full of pheromones" should {

      val p1 = FoodPheromone(Vector2D(3,3), decreasingFunction, startingIntensity)
      val p2 = FoodPheromone(Vector2D(-3,3), decreasingFunction, startingIntensity)
      val p3 = FoodPheromone(Vector2D(0,-10), decreasingFunction, startingIntensity)
      val zero = ZeroVector2D()
      val finalVector = ((p1.position - zero) * (startingIntensity / (p1.position --> zero))) >>
        ((p2.position - zero) * (startingIntensity / (p2.position --> zero))) >>
        ((p3.position - zero) * (startingIntensity / (p3.position --> zero)))

      val seq1 = Seq[FoodPheromone](p1, p2, p3)

      "calculate the weighted sum" in {
        assert(seq1.weightedSum(zero) == finalVector)
      }

    }
  }
}
