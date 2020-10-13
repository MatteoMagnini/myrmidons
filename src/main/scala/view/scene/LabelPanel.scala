package view.scene

import java.awt.Color

import view.Colors._

import scala.swing.{FlowPanel, Label, Separator}

/**
 * FlowPanel with short legend to understand color entities.
 */

private[view] case class LabelPanel() extends FlowPanel {

  private val foragingAnt = new Label("ForagingAnt")
  private val patrollingAnt = new Label("PatrollingAnt")
  private val food = new Label("Food")
  private val obstacle = new Label("Obstacle")
  private val anthill = new Label("Anthill")
  private val insect = new Label("Enemy")
  private val foodPheromone = new Label("FoodPheromone")
  private val dangerousPheromone = new Label("DangerousPheromone")
  private val separator = new Separator()
  /**
   * Each label refers to entities colors in simulation.
   */
  separator.background = ANT_COLOR

  food.foreground = new Color(FOOD_COLOR_COMPONENT._1,
    FOOD_COLOR_COMPONENT._2, FOOD_COLOR_COMPONENT._3)

  foragingAnt.foreground = ANT_COLOR

  patrollingAnt.foreground = PATROLLING_ANT_COLOR

  obstacle.foreground = OBSTACLE_COLOR

  insect.foreground = ENEMIES_COLOR

  anthill.foreground = new Color(ANTHILL_COLOR_COMPONENT._1,
    ANTHILL_COLOR_COMPONENT._2, ANTHILL_COLOR_COMPONENT._3)

  foodPheromone.foreground = new Color(FOOD_PHEROMONE_COLOR_COMPONENT._1,
    FOOD_PHEROMONE_COLOR_COMPONENT._2, FOOD_PHEROMONE_COLOR_COMPONENT._3)

  dangerousPheromone.foreground = new Color(DANGER_PHEROMONE_COLOR_COMPONENT._1,
    DANGER_PHEROMONE_COLOR_COMPONENT._2, DANGER_PHEROMONE_COLOR_COMPONENT._3)


  contents ++= Seq(foragingAnt, separator,
    patrollingAnt, separator,
    food, separator,
    obstacle, separator,
    anthill, separator,
    insect, separator,
    foodPheromone, separator,
    dangerousPheromone
  )
}
