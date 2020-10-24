import java.awt.{Color, Dimension, Toolkit}

import org.apache.commons.lang3.SystemUtils

package object view {

  val ANT_DRAW_SIZE = 4
  val PHEROMONE_DRAW_SIZE = 7
  val FIGHT_DRAW_SIZE = 20
  val SET_TO_CENTER = 2
  val OBSTACLE_DRAW_SIZE = 20
  var checkedSize: Dimension = Toolkit.getDefaultToolkit.getScreenSize
  if (SystemUtils.IS_OS_LINUX) {
    checkedSize = new Dimension(checkedSize.width, checkedSize.height - 30)
  }
  val SIMULATION_SIZE: Dimension = checkedSize
  val CENTER: (Int, Int) = (SIMULATION_SIZE.width / 2, SIMULATION_SIZE.height / 2)

  object Colors {

    val ANT_COLOR: Color = Color.BLACK

    val OBSTACLE_COLOR: Color = new Color(0.5f, 0.5f, 0.5f)

    val PATROLLING_ANT_COLOR: Color = new Color(0.5f, 0, 0)

    val ENEMIES_COLOR: Color = Color.RED

    val ANTHILL_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0.5f, 0f)
    val ANTHILL_COLOR: Color = new Color(ANTHILL_COLOR_COMPONENT._1,
      ANTHILL_COLOR_COMPONENT._2, ANTHILL_COLOR_COMPONENT._3)

    val FOOD_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.0f, 0.9f, 0.02f)
    val FOOD_PHEROMONE_COLOR: Color = new Color(FOOD_PHEROMONE_COLOR_COMPONENT._1,
      FOOD_PHEROMONE_COLOR_COMPONENT._2, FOOD_PHEROMONE_COLOR_COMPONENT._3)

    val DANGER_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.5f, 0.0f, 0.5f)
    val DANGER_PHEROMONE_COLOR: Color = new Color(DANGER_PHEROMONE_COLOR_COMPONENT._1,
      DANGER_PHEROMONE_COLOR_COMPONENT._2, DANGER_PHEROMONE_COLOR_COMPONENT._3)

    val FOOD_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0f, 1f)
    val FOOD_COLOR: Color = new Color(FOOD_COLOR_COMPONENT._1, FOOD_COLOR_COMPONENT._2, FOOD_COLOR_COMPONENT._3)

    /**
     * Set opacity of anthill object.
     *
     * @param anthillFood quantity of food in anthill.
     * @return right color for anthillFood value.
     */
    def ANTHILL_COLOR(anthillFood: Float): Color = {
      if (anthillFood < 0.3f) ANTHILL_COLOR_SET(0.3f) else ANTHILL_COLOR_SET(anthillFood)
    }

    private def ANTHILL_COLOR_SET(anthillFood: Float): Color = {
      new Color(ANTHILL_COLOR_COMPONENT._1,
        ANTHILL_COLOR_COMPONENT._2,
        ANTHILL_COLOR_COMPONENT._3, anthillFood)
    }

    /**
     * Set opacity of pheromone object.
     *
     * @param intensity intensity of pheromone.
     * @return right color for food pheromone value.
     */
    def FOOD_PHEROMONE_COLOR(intensity: Float): Color = {
      if (intensity < 0.3f) FOOD_PHEROMONE_COLOR_SET(0.3f) else FOOD_PHEROMONE_COLOR_SET(intensity)
    }

    /**
     * Set opacity of pheromone object.
     *
     * @param intensity intensity of pheromone.
     * @return right color for danger pheromone value.
     */
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

    /**
     * Set opacity of food object.
     *
     * @param quantity quantity of food.
     * @return right color for food value.
     */
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
