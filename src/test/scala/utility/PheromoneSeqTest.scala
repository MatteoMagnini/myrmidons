package utility

import model.environment.pheromones.{DangerPheromone, FoodPheromone, Pheromone}
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.{Vector2D, ZeroVector2D}

class PheromoneSeqTest extends AnyWordSpecLike with BeforeAndAfter {

  import utility.PheromoneSeq._

  "A seq of pheromones" when {

    val threshold = 1.0
    val DELTA = 1.0
    val decreasingFunction: Double => Double = x => x - DELTA
    val startingIntensity = 10.0

    "created" should {

      val seq1 = Seq[Pheromone]()

      "be empty" in {
        assert(seq1.isEmpty)
      }

      val p1 = FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity)
      val seq2 = seq1.add(p1)

      "correctly add a pheromone while seq is empty" in {
        assert(seq2.nonEmpty)
        assert(seq2.last == p1)
      }

      val seq3 = seq2.tick()

      "correctly tick with one pheromone" in {
        assert(seq3.last == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - DELTA))
      }

      val pos1 = Vector2D(1,1)
      val p2 = FoodPheromone(pos1, decreasingFunction, startingIntensity)
      val seq4 = seq3.add(p2, threshold)

      "correctly add a pheromone when seq is not empty" in {
        assert(seq4.nonEmpty)
        assert(seq4.size == 2)
        assert(seq4.last == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - DELTA))
        assert(seq4.head == p2)
      }

      val seq5 = seq4.tick()

      "correctly tick with two pheromones" in {
        assert(seq5.last == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - 2 * DELTA))
        assert(seq5.head == FoodPheromone(pos1, decreasingFunction, startingIntensity - DELTA))
      }

      val pos2 = Vector2D(0.75,1.25)
      val p3 = FoodPheromone(pos2, decreasingFunction, startingIntensity)
      val seq6 = seq5.add(p3, threshold)

      "correctly merge a pheromone when seq is not empty" in {
        assert(seq6.nonEmpty)
        assert(seq6.size == 2)
        assert(seq6.last == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - 2 * DELTA))
        assert(seq6.head == FoodPheromone(pos1, decreasingFunction, 2 * startingIntensity - DELTA))
      }

      var seq7 = seq6
      for(_ <- 0 to (startingIntensity.toInt * 2)) {
        seq7 = seq7.tick()
      }

      "after a lot of time pheromones evaporate" in {
        assert(seq7.isEmpty)
      }
    }

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

      val p4 = FoodPheromone(Vector2D(0,-10), decreasingFunction, startingIntensity)
      val seq2 = Seq(p1, p2, p3, p4)

      "return the pheromone with the strongest intensity" in {
        assert(seq2.strongest.get == p4)
      }

    }

    "two different pheromones" should {

      val dangerPheromone = DangerPheromone(ZeroVector2D(), x => x - DELTA, 10)
      val seq1 = Seq[Pheromone]().add(dangerPheromone, threshold)
      val foodPheromone = FoodPheromone(ZeroVector2D(), x => x - DELTA, 15)

      "add and not be merged" in {
        val result = seq1.add(foodPheromone)
        assert(result.size == 2)
      }
    }
  }
}
