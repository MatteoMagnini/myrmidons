package view

import scala.swing.TextField

package object scene {
  val DEFAULT_ANT_SIZE: Int = 140
  val DEFAULT_ANTHILL_FOOD: Int = 2000
  val DEFAULT_FOOD_SIZE: Int = 6
  val DEFAULT_OBSTACLE_SIZE: Int = 5
  val DEFAULT_ENEMIES_SIZE: Int = 50
  val PARAMETER_GRID: (Int, Int) = (6, 2)

  implicit def numberFrom(component: TextField): Int = {
    if (component.text == "") {
      component.text = MIN_COMPONENT
    }
    component.text.toInt
  }
}
