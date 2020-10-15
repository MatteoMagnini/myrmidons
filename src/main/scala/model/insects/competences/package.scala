package model.insects

import scala.util.Random

package object competences {

  val NOISE = 0.1
  val MAX_VELOCITY: Double = 5
  val MIN_VELOCITY: Double = 2
  val INERTIA_FACTOR: Double = 0.9
  val INERTIA_FACTOR_IN_TAXIS: Double = 2.0
  val FOOD_EATEN_PER_STEP: Double = 0.5
  val ENERGY_RANDOM_WALK: Double = -0.1
  val ENERGY_PICK_FOOD: Double = -0.2
  val ENERGY_EATING: Double = -0.1
  val ENERGY_STORE_FOOD: Double = -0.2
  val ENERGY_FOOD_PHEROMONE_TAXIS: Double = -0.1
  val ENERGY_DANGER_PHEROMONE_TAXIS: Double = -0.1
  val THRESHOLD_GO_BACK_HOME: Double = 40.0
  val THRESHOLD_GO_OUTSIDE: Double = 80.0
  val RANDOM: Random.type = scala.util.Random

  def random(probability: Double): Boolean =
    RANDOM.nextDouble() < probability
}
