package utility

import utility.geometry.{Vector2D, ZeroVector2D}
import scala.util.Random

object Parameters {

  object Pheromones {

    object FoodPheromoneInfo {
      val DELTA: Double = 3.0
      val FOOD_PHEROMONE_MERGING_THRESHOLD: Double = 5.0
      val STARTING_INTENSITY: Double = 100.0
      val MAX_INTENSITY: Double = 1000.0
      val INTENSITY_FACTOR: Double = 3.0
    }

    object DangerPheromoneInfo {
      val DELTA: Double = 4.0
      val DANGER_PHEROMONE_MERGING_THRESHOLD: Double = 2.0
      val STARTING_INTENSITY: Double = 100.0
      val MAX_INTENSITY: Double = 3000.0
      val INTENSITY_FACTOR: Double = 5.0
    }
  }

  object Insects {

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
        val DANGER_PHEROMONE_RANGE = 20.0
        val FOOD_ENERGY_CONVERSION = 10.0
        val STARTING_ENERGY = 150.0
        val STARTING_TIME = 0
        val STARTING_POSITION: Vector2D = ZeroVector2D()
      }

    }

    object Enemy {
      val MIN_ENERGY = 65
      val MAX_ENERGY = 85
      val STARTING_TIME = 0
      val ENERGY: Double = MIN_ENERGY + (MAX_ENERGY - MIN_ENERGY) * Random.nextDouble()
      val STARTING_POSITION: Vector2D = ZeroVector2D()
    }
  }

  object GUIConstant {
    val ANT_SIZE = 4
    val PHEROMONE_SIZE = 7
    val FIGHT_SIZE = 20
    val SET_TO_CENTER = 2
    val OBSTACLE_SIZE = 20
    val SIMULATION_SIZE: (Int, Int) = (800,900)
    val SIMULATION_BOUNDARY: (Int, Int) = (800,800)
    val SETTING_SIZE = 400
    val MIN_COMPONENT = 6
  }

  object Competence {
    val NOISE = 0.1
    val MAX_VELOCITY: Double = 5
    val MIN_VELOCITY: Double = 2
    val INERTIA_FACTOR: Double = 0.9
    val FOOD_EATEN_PER_STEP: Double = 0.5
    val ENERGY_RANDOM_WALK: Double = -0.3
    val ENERGY_PICK_FOOD: Double = -0.2
    val ENERGY_EATING: Double = -0.1
    val ENERGY_STORE_FOOD: Double = -0.1
    val ENERGY_FOOD_PHEROMONE_TAXIS: Double = -0.3
    val ENERGY_DANGER_PHEROMONE_TAXIS: Double = -0.2
    val RANDOM: Random.type = scala.util.Random
  }

  object Environment {
    val ANTHILL_RADIUS = 15
    val FOOD_AMOUNT = 2000
    val FOOD_RADIUS: (Int, Int) = (100, 150)
  }

}
