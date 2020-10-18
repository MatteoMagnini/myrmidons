package model.environment

package object pheromones {

  val INFLUENCE_RADIUS = 10.0

  object FoodPheromoneInfo {
    val DELTA: Double = 1.0
    val FOOD_PHEROMONE_MERGING_THRESHOLD: Double = 5.0
    val STARTING_INTENSITY: Double = 100.0
    val MAX_INTENSITY: Double = 1000.0
    val DECREASING_FACTOR: Double = 0.999
    val INTENSITY_FACTOR: Double = 3.0
  }

  object DangerPheromoneInfo {
    val DELTA: Double = 4.0
    val DANGER_PHEROMONE_MERGING_THRESHOLD: Double = 3.0
    val STARTING_INTENSITY: Double = 100.0
    val MAX_INTENSITY: Double = 1000.0
    val INTENSITY_FACTOR: Double = 5.0
  }
}
