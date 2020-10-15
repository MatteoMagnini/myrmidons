package view.scene

import view.Colors._

import scala.swing.{FlowPanel, Label, Separator}

trait LabelPanel extends FlowPanel {}

object LabelPanel {

  def apply(): LabelPanel = new LabelPanelImpl()

  /**
   * FlowPanel with short legend to understand color entities.
   */
  private[view] class LabelPanelImpl() extends LabelPanel {

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

    food.foreground = FOOD_COLOR
    foragingAnt.foreground = ANT_COLOR
    patrollingAnt.foreground = PATROLLING_ANT_COLOR
    obstacle.foreground = OBSTACLE_COLOR
    insect.foreground = ENEMIES_COLOR
    anthill.foreground = ANTHILL_COLOR
    foodPheromone.foreground = FOOD_PHEROMONE_COLOR
    dangerousPheromone.foreground = DANGER_PHEROMONE_COLOR


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

}

