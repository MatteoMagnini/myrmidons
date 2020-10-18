package view

import java.awt.Toolkit

import scala.swing.TextField

package object scene {
  val DEFAULT_ANT_SIZE: Int = 140
  val DEFAULT_ANTHILL_FOOD: Int = 2000
  val DEFAULT_FOOD_SIZE: Int = 6
  val DEFAULT_OBSTACLE_SIZE: Int = 5
  val DEFAULT_ENEMIES_SIZE: Int = 50
  val PARAMETER_GRID: (Int, Int) = (6, 2)
  val MIN_COMPONENT = "6"
  val SIMULATION_BOUNDARY = Toolkit.getDefaultToolkit().getScreenSize
  val CENTER = (SIMULATION_BOUNDARY.width / 2, SIMULATION_BOUNDARY.height / 2)
  val MIN_ZOOM = 0.25
  val MAX_ZOOM = 4.0
  val ZOOM_STEP = 0.25
  val MIN_SPACE = 1.0
  val STEP_LENGTH = 50.0

  implicit def numberFrom(component: TextField): Int = {
    if (component.text == "") {
      component.text = MIN_COMPONENT
    }
    component.text.toInt
  }
}
