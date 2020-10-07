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

  private def FOOD_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.0f, 0.9f, 0.02f)

  private def DANGER_PHEROMONE_COLOR_COMPONENT: (Float, Float, Float) = (0.5f, 0.0f, 0.5f)

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

