package utility

import model.environment.pheromones.{DangerPheromone, FoodPheromone, Pheromone}
import org.scalatest.BeforeAndAfter
import org.scalatest.wordspec.AnyWordSpecLike
import utility.geometry.{Vector2D, ZeroVector2D}

class PheromoneMapTest extends AnyWordSpecLike with BeforeAndAfter {

  import utility.PheromoneMap._

  "A map of pheromones" when {

    val threshold = 1.0
    val DELTA = 1.0
    val decreasingFunction: Double => Double = x => x - DELTA
    val startingIntensity = 10.0

    "created" should {

      val map1 = Map[Int,Pheromone]()

      "be empty" in {
        assert(map1.isEmpty)
      }

      val p1 = FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity)
      val map2 = map1.add(p1)

      "correctly add a pheromone while seq is empty" in {
        assert(map2.nonEmpty)
        assert(map2.head._2 == p1)
      }

      val map3 = map2.tick()

      "correctly tick with one pheromone" in {
        assert(map3.last._2 == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - DELTA))
      }

      val pos1 = Vector2D(1,1)
      val p2 = FoodPheromone(pos1, decreasingFunction, startingIntensity)
      val map4 = map3.add(p2, threshold)

      "correctly add a pheromone when seq is not empty" in {
        assert(map4.nonEmpty)
        assert(map4.size == 2)
        assert(map4.head._2 == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - DELTA))
        assert(map4.last._2 == p2)
      }

      val map5 = map4.tick()

      "correctly tick with two pheromones" in {
        assert(map5.head._2 == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - 2 * DELTA))
        assert(map5.last._2 == FoodPheromone(pos1, decreasingFunction, startingIntensity - DELTA))
      }

      val pos2 = Vector2D(0.75,1.25)
      val p3 = FoodPheromone(pos2, decreasingFunction, startingIntensity)
      val map6 = map5.add(p3, threshold)

      "correctly merge a pheromone when seq is not empty" in {
        assert(map6.nonEmpty)
        assert(map6.size == 2)
        assert(map6.head._2 == FoodPheromone(ZeroVector2D(), decreasingFunction, startingIntensity - 2 * DELTA))
        assert(map6.last._2 == FoodPheromone(pos1, decreasingFunction, 2 * startingIntensity - DELTA))
      }

      var map7 = map6
      for(_ <- 0 to (startingIntensity.toInt * 2)) {
        map7 = map7.tick()
      }

      "after a lot of time pheromones evaporate" in {
        assert(map7.isEmpty)
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

      val map1 = Map[Int,FoodPheromone](1 -> p1, 2 -> p2, 3 -> p3)

      "calculate the weighted sum" in {
        assert(map1.weightedSum(zero) == finalVector)
      }

      val p4 = FoodPheromone(Vector2D(0,-10), decreasingFunction, startingIntensity)
      val map2 = Map(1 -> p1, 2 -> p2, 3 -> p3, 4 -> p4)

      "return the pheromone with the strongest intensity" in {
        assert(map2.strongest.get == p4)
      }

    }

    "two different pheromones" should {

      val dangerPheromone = DangerPheromone(ZeroVector2D(), x => x - DELTA, 10)
      val map1 = Map[Int,Pheromone]().add(dangerPheromone, threshold)
      val foodPheromone = FoodPheromone(ZeroVector2D(), x => x - DELTA, 15)

      "add and not be merged" in {
        val result = map1.add(foodPheromone)
        assert(result.size == 2)
      }
    }
  }
}
