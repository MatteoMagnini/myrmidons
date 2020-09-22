package utility

import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.Geometry.{Vector2D, ZeroVector2D}

class PheromoneSeqTest extends AnyWordSpecLike with BeforeAndAfter {

  import utility.PheromoneSeq._
  import model.environment._

  "A seq of pheromones" when {

    val threshold = 1.0
    val DELTA = 1.0
    val startingIntensity = 100.0

    "created" should {

      val seq1 = Seq[Pheromone]()

      "be empty" in {
        assert(seq1.isEmpty)
      }

      val p1 = FoodPheromone(ZeroVector2D(), DELTA, startingIntensity)
      val seq2 = seq1.add(p1)

      "correctly add a pheromone while seq is empty" in {
        assert(seq2.nonEmpty)
        assert(seq2.last == p1)
      }

      val seq3 = seq2.tick()

      "correctly tick with one pheromone" in {
        assert(seq3.last == FoodPheromone(ZeroVector2D(), DELTA, startingIntensity - DELTA))
      }

      val pos1 = Vector2D(1,1)
      val p2 = FoodPheromone(pos1,DELTA,startingIntensity)
      val seq4 = seq3.add(p2, threshold)

      "correctly add a pheromone when seq is not empty" in {
        assert(seq4.nonEmpty)
        assert(seq4.size == 2)
        assert(seq4.last == FoodPheromone(ZeroVector2D(), DELTA, startingIntensity - DELTA))
        assert(seq4.take(1).last == p2)
      }

      val seq5 = seq4.tick()

      "correctly tick with two pheromones" in {
        assert(seq5.last == FoodPheromone(ZeroVector2D(), DELTA, startingIntensity - 2 * DELTA))
        assert(seq5.take(1).last == FoodPheromone(pos1, DELTA, startingIntensity - DELTA))
      }

      val pos2 = Vector2D(0.75,1.25)
      val p3 = FoodPheromone(pos2,DELTA,startingIntensity)
      val seq6 = seq5.add(p3, threshold)

      "correctly merge a pheromone when seq is not empty" in {
        assert(seq6.nonEmpty)
        assert(seq6.size == 2)
        assert(seq6.last == FoodPheromone(ZeroVector2D(), DELTA, startingIntensity - 2 * DELTA))
        assert(seq6.take(1).last == FoodPheromone(pos1,DELTA, 2 * startingIntensity - DELTA))
      }

      var seq7 = seq6
      for(_ <- 0 to (startingIntensity.toInt * 2)) {
        seq7 = seq7.tick()
      }

      "after a lot of time pheromones evaporate" in {
        assert(seq7.isEmpty)
      }
    }
  }
}
