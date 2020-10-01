package view.ColorUtility

import java.awt.Color

object Colors {

  def ANT_COLOR: Color = Color.BLACK

  def OBSTACLE_COLOR: Color = new Color(0.5f, 0.5f, 0.5f, 0.5f)

  def ENEMIES_COLOR: Color = Color.RED

  private def ANTHILL_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0.5f, 0f)

  def ANTHILL_COLOR(anthillFood: Float): Color = {
    if (anthillFood < 0.3f) ANTHILL_COLOR_SET(0.3f) else ANTHILL_COLOR_SET(anthillFood)
  }

  private def ANTHILL_COLOR_SET(anthillFood: Float): Color = {
    new Color(ANTHILL_COLOR_COMPONENT._1,
      ANTHILL_COLOR_COMPONENT._2,
      ANTHILL_COLOR_COMPONENT._3, anthillFood)
  }

  private def PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (1f, 0.1f, 0.02f)

  def PHEROMONE_COLOR(intensity: Float): Color = {
    if (intensity < 0.3f) PHEROMONE_COLOR_SET(0.3f) else PHEROMONE_COLOR_SET(intensity)
  }

  private def PHEROMONE_COLOR_SET(intensity: Float): Color = {
    new Color(PHEROMONE_COLOR_COMPONENT._1,
      PHEROMONE_COLOR_COMPONENT._2,
      PHEROMONE_COLOR_COMPONENT._3, intensity)
  }

  private def FOOD_COLOR_COMPONENT: (Float, Float, Float) = (0f, 0f, 1f)

  def FOOD_COLOR(quantity: Float): Color = {
    if (quantity < 0.4f) FOOD_COLOR_SET(0.4f) else FOOD_COLOR_SET(quantity)
  }

  private def FOOD_COLOR_SET(quantity: Float): Color = {
    new Color(FOOD_COLOR_COMPONENT._1,
      FOOD_COLOR_COMPONENT._2,
      FOOD_COLOR_COMPONENT._3, quantity)
  }

}

