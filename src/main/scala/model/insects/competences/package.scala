package model.insects

import scala.util.Random

package object competences {

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
