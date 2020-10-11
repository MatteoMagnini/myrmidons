package model.environment

package object pheromones {

  object FoodPheromoneInfo {
    val DELTA: Double = 1.5
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
