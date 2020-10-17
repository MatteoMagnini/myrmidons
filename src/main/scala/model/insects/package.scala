package model

import utility.geometry.{Vector2D, ZeroVector2D}
import scala.util.Random

package object insects {

  object Ants {

    object ForagingAnt {
      val MAX_ENERGY = 100.0
      val FOOD_PHEROMONE_RANGE = 10.0
      val DANGER_PHEROMONE_RANGE = 5.0 // TODO: for now not used, in future foraging ants could avoid danger...
      val MAX_FOOD = 10.0
      val FOOD_ENERGY_CONVERSION = 10.0
      val STARTING_ENERGY = 100.0
      val STARTING_TIME = 0
      val STARTING_FOOD_AMOUNT = 0.0
      val STARTING_POSITION: Vector2D = ZeroVector2D()
    }

    object PatrollingAnt {
      val MAX_ENERGY = 150.0
      val DANGER_PHEROMONE_RANGE = 10.0
      val FOOD_ENERGY_CONVERSION = 10.0
      val STARTING_ENERGY = 150.0
      val STARTING_TIME = 0
      val STARTING_POSITION: Vector2D = ZeroVector2D()
    }

  }

  object Enemies {
    val MIN_ENERGY = 50
    val MAX_ENERGY = 70
    val STARTING_TIME = 0
    val ENERGY: Double = MIN_ENERGY + (MAX_ENERGY - MIN_ENERGY) * Random.nextDouble()
    val STARTING_POSITION: Vector2D = ZeroVector2D()
  }

}
