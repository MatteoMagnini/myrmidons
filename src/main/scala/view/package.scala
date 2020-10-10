import java.awt.Color

package object view {

  val ANT_SIZE = 4
  val PHEROMONE_SIZE = 7
  val FIGHT_SIZE = 20
  val SET_TO_CENTER = 2
  val OBSTACLE_SIZE = 20
  val SIMULATION_SIZE: (Int, Int) = (800, 900)
  val SIMULATION_BOUNDARY: (Int, Int) = (800, 800)
  val SETTING_SIZE = 400
  val MIN_COMPONENT = 6

  object Colors {

    def ANT_COLOR: Color = Color.BLACK

    def OBSTACLE_COLOR: Color = new Color(0.5f, 0.5f, 0.5f, 0.5f)

    def ENEMIES_COLOR: Color = Color.RED

    def ANTHILL_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0.5f, 0f)

    def PATROLLING_ANT_COLOR = new Color(0.5f, 0, 0)

    def FOOD_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.0f, 0.9f, 0.02f)

    def DANGER_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.5f, 0.0f, 0.5f)


    def ANTHILL_COLOR(anthillFood: Float): Color = {
      if (anthillFood < 0.3f) ANTHILL_COLOR_SET(0.3f) else ANTHILL_COLOR_SET(anthillFood)
    }

    def FOOD_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0f, 1f)

    private def ANTHILL_COLOR_SET(anthillFood: Float): Color = {
      new Color(ANTHILL_COLOR_COMPONENT._1,
        ANTHILL_COLOR_COMPONENT._2,
        ANTHILL_COLOR_COMPONENT._3, anthillFood)
    }

    def FOOD_PHEROMONE_COLOR(intensity: Float): Color = {
      if (intensity < 0.3f) FOOD_PHEROMONE_COLOR_SET(0.3f) else FOOD_PHEROMONE_COLOR_SET(intensity)
    }

    def DANGER_PHEROMONE_COLOR(intensity: Float): Color = {
      if (intensity < 0.3f) DANGER_PHEROMONE_COLOR_SET(0.3f) else DANGER_PHEROMONE_COLOR_SET(intensity)
    }

    private def FOOD_PHEROMONE_COLOR_SET(intensity: Float): Color = {
      new Color(FOOD_PHEROMONE_COLOR_COMPONENT._1,
        FOOD_PHEROMONE_COLOR_COMPONENT._2,
        FOOD_PHEROMONE_COLOR_COMPONENT._3, intensity)
    }

    private def DANGER_PHEROMONE_COLOR_SET(intensity: Float): Color = {
      new Color(DANGER_PHEROMONE_COLOR_COMPONENT._1,
        DANGER_PHEROMONE_COLOR_COMPONENT._2,
        DANGER_PHEROMONE_COLOR_COMPONENT._3, intensity)
    }

    def FOOD_COLOR(quantity: Float): Color = {
      if (quantity < 0.4f) FOOD_COLOR_SET(0.4f) else FOOD_COLOR_SET(quantity)
    }

    private def FOOD_COLOR_SET(quantity: Float): Color = {
      new Color(FOOD_COLOR_COMPONENT._1,
        FOOD_COLOR_COMPONENT._2,
        FOOD_COLOR_COMPONENT._3, quantity)
    }

  }

}
