package view.scene

import java.awt.Color
import scala.swing.{FlowPanel, Label, Separator}

/**
 * FlowPanel with short legend to understand color entities.
 */

private[view] case class LabelPane() extends FlowPanel {

  private val ant = new Label("Ant")
  private val food = new Label("Food")
  private val obstacle = new Label("Obstacle")
  private val anthill = new Label("Anthill")
  private val insect = new Label("Insect")
  private val pheromone = new Label("Pheromone")
  private val separator = new Separator()
  /**
   * Each label refers to entities colors in simulation.
   */
  separator.background = Color.black
  food.foreground = Color.blue
  ant.foreground = Color.black
  obstacle.foreground = Color.gray
  insect.foreground = Color.red
  anthill.foreground = new Color(0f, 0.5f, 0f, 1f)
  pheromone.foreground = Color.magenta

  contents ++= Seq(ant, separator,
    food, separator,
    obstacle, separator,
    anthill, separator,
    insect, separator, pheromone)
}
