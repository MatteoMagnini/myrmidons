package utility

import utility.geometry.{Vector2D, ZeroVector2D}
import scala.util.Random

object Parameters {

  object ForagingAnt {
    def MAX_ENERGY = 100.0
    def MAX_FOOD = 10.0
    def FOOD_ENERGY_CONVERSION = 10.0
    def STARTING_ENERGY = 100.0
    def STARTING_TIME = 0
    def STARTING_FOOD_AMOUNT = 0.0
    def STARTING_POSITION: Vector2D = ZeroVector2D()
  }

  object GUIConstant {
    def ANT_SIZE = 4
    def PHEROMONE_SIZE = 7
    def FIGHT_SIZE = 20
    def SET_TO_CENTER = 2
    def OBSTACLE_SIZE = 20
  }

  object Competence {
    val NOISE = 0.1
    val FOOD_PHEROMONE_THRESHOLD: Double = 5.0
    val ANT_PHEROMONE_THRESHOLD: Double = 10.0
    val MAX_VELOCITY: Double = 5
    val MIN_VELOCITY: Double = 2
    val INERTIA_FACTOR: Double = 0.9
    val FOOD_EATEN_PER_STEP: Double = 0.5
    val ENERGY_RW: Double = -0.3
    val ENERGY_PF: Double = -0.2
    val ENERGY_EATING: Double = -0.1
    val ENERGY_SF: Double = -0.1
    val ENERGY_FPT: Double = -0.3
    val RANDOM: Random.type = scala.util.Random
  }

}
